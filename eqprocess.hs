module EqProcess where

import Data.Complex
import YC
import VolSurf
import RNG

data Lognormal a = Lognormal { volatility :: a } deriving (Show, Eq)

data Heston a = Heston { v0 :: a,
			 vf :: a, 
			 kappa :: a, 
			 volvol :: a, 
			 rho :: a } deriving (Show, Eq)

data Dupire a = Dupire { rf :: YieldCurve a,
			 div :: YieldCurve a,
			 s0  :: a,
			 vols :: VolSurf a}

data MCState a = MCState { model :: a Double, esgstate :: PrefetchRands }

class CharFunc a where
	charfuncfactory :: (RealFloat b)=>a b->b->(Complex b->Complex b)

instance CharFunc Lognormal where
	charfuncfactory (Lognormal volatility) t = f
		where 	
			f x = exp $ -i*vol*vol/2.0*tc*x-vol*vol*x*x/2.0*tc
			i = 0.0:+1.0
			tc = t:+0.0
			vol = volatility:+0.0

instance CharFunc Heston where
	charfuncfactory (Heston v0 vf kappa volvol rho) t = f 
		where
			f x = exp $ (b x + c x)
			zeta x = -(x*x+i*x)/2.0
			gamma x = kapc-rhoc*volvolc*x*i
			psi x = sqrt $ (gamma x) * (gamma x)-2.0*vol*vol*zeta x
			btop x = 2.0*zeta x*(1.0-exp (-psi x*tc))*vol
			bbottom x = 2.0*psi x-(psi x - gamma x)*(1.0-exp(-tc*psi x))
			b x = btop x / bbottom x
			logterm x = (2.0*psi x-(psi x-gamma x)*(1.0-exp(-tc*psi x)))/2.0/(psi x)
			c x = -kapc*vfc/volvolc/volvolc*(2.0*log(logterm x)+(psi x-gamma x)*tc)
			convert x = x:+0.0
			i = 0.0:+1.0
			tc = convert t
			vol = convert v0
			kapc = convert kappa
			rhoc = convert rho
			vfc = convert vf
			volvolc = convert volvol

class Discretize a where
	evolve ::  MCState a->         --model + rng
		   YieldCurve Double-> --rf curve
		   YieldCurve Double-> --carry curve
		   Double->            --start level
		   Double->            --start time
		   Double->            --end time
		   (Double, MCState a) --final value and state

	genpath :: MCState a->         --model + rng
		   YieldCurve Double-> --risk-free rate
		   YieldCurve Double-> --carry curve
		   Double->            --start level
		   Double->            --start time
		   Double->            --end time
		   Int->               --steps
		   ([Double], MCState a)  --final model + rng
	genpath initstate rf div lvl t1 t2 intervals = genpath' [lvl] initstate t1 intervals
		where
			genpath' lvls mcs start 1 = (lvls++[newlvl], mcs')
				where
					(newlvl, mcs') = evolve mcs rf div (last lvls) start t2
			genpath' lvls mcs start n = genpath' (lvls++[newlvl]) mcs' (start+dt) (n-1)
				where
					(newlvl, mcs') = evolve mcs rf div (last lvls) start (start+dt)
					dt = (t2-start)/(fromIntegral n)


instance Discretize Lognormal where
	evolve (MCState (Lognormal vol) rng) rf div lvl t1 t2 = (newlvl, MCState (Lognormal vol) newrng)
		where
			(rand, newrng) = fetchrand rng Normal
			t = t2-t1
			r = forward rf t1 t2 - forward div t1 t2
			rate = (*) t (r-vol*vol*t/2.0)
			newlvl = lvl*exp (rate+rand*vol*(sqrt t))

instance Discretize Dupire where
	evolve (MCState dp rng) rf div lvl t1 t2 = (newlvl, MCState dp newrng)
		where
			lvol = localvol dp lvl t1
			(newlvl, MCState _ newrng) = evolve (MCState (Lognormal lvol) rng) rf div lvl t1 t2

instance Discretize Heston where
	evolve (MCState (Heston vinit vfinal kap sigma correl) rng) rf div lvl t1 t2 = (newlvl, MCState (Heston flipv vfinal kap sigma correl) newrng)
		where
			([x,y], newrng) = fetchrands rng Normal 2
			z = correl*x+sqrt (1-correl*correl)*y
			r = forward rf t1 t2 - forward div t1 t2
			t = t2-t1
			newlvl = lvl * exp (r-vinit/2.0+x*sqrt (vinit*t))
			newv = (sqrt vinit+sigma/2.0*(sqrt t)*z)^2-kap*(vinit-vfinal)*t-sigma*sigma*t/4.0
			flipv = if newv>0.0 then newv else -newv


localvol (Dupire riskf div s vs) k t | w==0.0 || solution<0.0 = sqrt dwdt
				     | otherwise = sqrt solution
	where
		fwd t1 t2 = exp $ (forward riskf t1 t2) - (forward div t1 t2)
		f = s*(fwd 0.0 t)
		y = log $ k/f
		dy = 1.0E-6
		[kp, km] = map (*k) [dy, 1/dy]
		[w, wp, wm] = map var [k/s, kp/s, km/s]
		dwdy = (wp-wm)/2.0/dy
		d2wdy2 = (wp-2.0*w+wm)/dy/dy
		var a = variance vs a t
		dt = min 0.0001 (t/2.0)
		dwdt = case t of
			0.0->(var (k*fwd t (t+1E-5))-w)/dt
			otherwise->(var (strikept/s)-var (strikemt/s))/2.0/dt
				where 
					strikept = k*fwd t (t+dt)
					strikemt = k/(fwd t (t-dt))
		solution = dwdt/(1.0-y/w*dwdy+0.25*(-0.25-1.0/w+y*y/w/w)*dwdy*dwdy+0.5*d2wdy2)

