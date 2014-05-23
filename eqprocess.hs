module EqProcess where

import Data.Complex
import YC
import VolSurf
import RNG
import Control.Monad.State
import qualified Data.Vector.Unboxed as U

data Lognormal a = Lognormal { lnvolatility :: a } deriving (Show, Eq)

data Heston a = Heston { v0 :: a,
			 vf :: a, 
			 kappa :: a, 
			 volvol :: a, 
			 rho :: a } deriving (Show, Eq)

data Dupire a = Dupire { vols :: VolSurf a,
			 s0   :: a} deriving (Show, Eq)

data MCState a = MCState { model :: a Double, 
			   rngstate :: PrefetchRands, 
			   lvlstate :: Double }

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
	evolve ::  YieldCurve Double->
		   YieldCurve Double->
		   Double->            
		   Double->
		   State (MCState a) Double 

	genpath :: YieldCurve Double->
		   YieldCurve Double->
		   Double->            
		   Double->            
		   Int->               
		   State (MCState a) [Double]
	genpath rf div t1 t2 intervals = do
		let
			genpath' lvls start 1 = do
				newlevel <- evolve rf div start t2
				return (lvls++[newlevel])
			genpath' lvls start n = do
				let dt = (t2-start)/(fromIntegral n)
				newlvl <- evolve rf div start (start+dt)
				solution <- genpath' (lvls++[newlvl]) (start+dt) (n-1)
				return solution
		result<-genpath' [] t1 intervals
		return result
				

randWrapper :: Distribution->State (MCState a) Double
randWrapper x = state $ f
	where 
		f (MCState a1 myrng a3) = (randval, MCState a1 newrng a3)
			where (randval, newrng) = fetchrand myrng x

instance Discretize Lognormal where
	evolve rf div t1 t2 = do
		rand<-randWrapper Normal
		internal<-get
		let 
			(model, rng, lvl) = unwrap internal
			t = t2-t1
			r = forward rf t1 t2 - forward div t1 t2
			vol = lnvolatility model
			rate = (*) t (r-vol*vol*t/2.0)
			newlvl = lvl*exp (rate+rand*vol*(sqrt t))
		put $ MCState model rng newlvl
		return $ newlvl

instance Discretize Dupire where
	evolve rf div t1 t2 = do
		rand<-randWrapper Normal
		internal<-get
		let 
			(model, rng, lvl) = unwrap internal
			t = t2-t1
			r = forward rf t1 t2 - forward div t1 t2
			lvol = localvol model rf div lvl t1
			rate = (*) t (r-lvol*lvol*t/2.0)
			newlvl = lvl*exp (rate+rand*lvol*(sqrt t))
		put $ MCState model rng newlvl
		return $ newlvl

instance Discretize Heston where
	evolve rf div t1 t2 = do
		x<-randWrapper Normal
		y<-randWrapper Normal
		internal<-get
		let 
			(model, rng, lvl) = unwrap internal
			(vinit, vfinal, kap, sigma, correl) = (\(Heston a b c d e)->(a,b,c,d,e)) model
			z = x*correl+sqrt(1-correl*correl)*y
			r = forward rf t1 t2 - forward div t1 t2
			t = t2-t1
			newlvl = lvl * exp (r-vinit/2.0+x*sqrt (vinit*t))
			newv = (sqrt vinit+sigma/2.0*(sqrt t)*z)^2-kap*(vinit-vfinal)*t-sigma*sigma*t/4.0
			flipv = if newv>0.0 then newv else -newv
		put (MCState (Heston flipv vfinal kap sigma correl) rng newlvl)
		return newlvl

unwrap :: MCState t-> (t Double, PrefetchRands, Double)
unwrap (MCState a b c) = (a,b,c)

localvol :: (RealFloat a, U.Unbox a)=>Dupire a->YieldCurve a->YieldCurve a->a->a->a
localvol (Dupire vs s) rcurve dcurve k t | w==0.0 || solution<0.0 = sqrt dwdt
			                 | otherwise = sqrt solution
	where
		fwd t1 t2 = exp $ (forward rcurve t1 t2) - (forward dcurve t1 t2)
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
