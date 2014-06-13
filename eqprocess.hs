module EqProcess where

import Data.Complex
import YC
import VolSurf
import RNG
import Control.Monad.State

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
	charfuncfactory :: (RealFloat b)=>a b->b->Complex b->Complex b

instance CharFunc Lognormal where
	charfuncfactory (Lognormal v) t = f
		where 	
			f x = exp $ -i*vol*vol/2.0*tc*x-vol*vol*x*x/2.0*tc
			i = 0.0:+1.0
			tc = t:+0.0
			vol = v:+0.0

instance CharFunc Heston where
	charfuncfactory (Heston v vt k sig corr) t = f 
		where
			f x = exp (b x + c x)
			zeta x = -(x*x+i*x)/2.0
			gamma x = kapc-rhoc*volvolc*x*i
			psi x = sqrt $ gamma x * gamma x-2.0*vol*vol*zeta x
			btop x = 2.0*zeta x*(1.0-exp (-psi x*tc))*vol
			bbottom x = 2.0*psi x-(psi x - gamma x)*(1.0-exp(-tc*psi x))
			b x = btop x / bbottom x
			logterm x = (2.0*psi x-(psi x-gamma x)*(1.0-exp(-tc*psi x)))/2.0/psi x
			c x = -kapc*vfc/volvolc/volvolc*(2.0*log(logterm x)+(psi x-gamma x)*tc)
			convert x = x:+0.0
			i = 0.0:+1.0
			tc = convert t
			vol = convert v
			kapc = convert k
			rhoc = convert corr
			vfc = convert vt
			volvolc = convert sig

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
	genpath rf dv t1 t2 intervals = do
		let
			genpath' lvls start 1 = do
				newlevel <- evolve rf dv start t2
				return (lvls++[newlevel])
			genpath' lvls start n = do
				let dt = (t2-start)/fromIntegral n
				newlvl <- evolve rf dv start (start+dt)
				genpath' (lvls++[newlvl]) (start+dt) (n-1)
		genpath' [] t1 intervals
				

randWrapper :: Distribution->State (MCState a) Double
randWrapper x = state f
	where 
		f (MCState a1 myrng a3) = (randval, MCState a1 newrng a3)
			where (randval, newrng) = fetchrand myrng x

instance Discretize Lognormal where
	evolve rf dv t1 t2 = do
		rand<-randWrapper Normal
		internal<-get
		let 
			(modl, rng, lvl) = unwrap internal
			t = t2-t1
			r = forward rf t1 t2 - forward dv t1 t2
			vol = lnvolatility modl
			rt = (*) t (r-vol*vol*t/2.0)
			newlvl = lvl*exp (rt+rand*vol*sqrt t)
		put $ MCState modl rng newlvl
		return newlvl

instance Discretize Dupire where
	evolve rf dv t1 t2 = do
		rand<-randWrapper Normal
		internal<-get
		let 
			(modl, rng, lvl) = unwrap internal
			t = t2-t1
			rt = forward rf t1 t2 - forward dv t1 t2
			lvol = localvol modl rf dv lvl t1
			r = (*) t (rt-lvol*lvol*t/2.0)
			newlvl = lvl*exp (r+rand*lvol*sqrt t)
		put $ MCState modl rng newlvl
		return newlvl

instance Discretize Heston where
	evolve rf dv t1 t2 = do
		x<-randWrapper Normal
		y<-randWrapper Normal
		internal<-get
		let 
			(modl, rng, lvl) = unwrap internal
			(vinit, vfinal, kap, sigma, correl) = (\(Heston a b c d e)->(a,b,c,d,e)) modl
			z = x*correl+sqrt(1-correl*correl)*y
			r = forward rf t1 t2 - forward dv t1 t2
			t = t2-t1
			newlvl = lvl * exp (r-vinit/2.0+x*sqrt (vinit*t))
			newv = (sqrt vinit+sigma/2.0*z*sqrt t)^2-kap*(vinit-vfinal)*t-sigma*sigma*t/4.0
			flipv = if newv>0.0 then newv else -newv
		put (MCState (Heston flipv vfinal kap sigma correl) rng newlvl)
		return newlvl

unwrap :: MCState t-> (t Double, PrefetchRands, Double)
unwrap (MCState a b c) = (a,b,c)

localvol :: (RealFloat a)=>Dupire a->YieldCurve a->YieldCurve a->a->a->a
localvol (Dupire vs s) rcurve dcurve k t | w==0.0 || solution<0.0 = sqrt dwdt
			                 		     | otherwise = sqrt solution
	where
		discs tmat = (disc rcurve tmat, disc dcurve tmat)
		(dr,dq) = discs t		
		f = s*dq/dr
		y = log $ k/f
		dy = 1.0E-6
		[kp, km] = map (*k) [exp dy, 1/exp dy]
		[w, wp, wm] = map (\x->var (x/s) t) [k, kp, km]
		dwdy = (wp-wm)/2.0/dy
		d2wdy2 = (wp-2.0*w+wm)/dy/dy
		var = variance vs
		dt = min 0.0001 (t/2.0)
		dwdt = let 
				strikept = k*dr*dqpt/drpt/dq
				strikemt = k*dr*dqmt/drmt/dq
				(drpt, dqpt) = discs $ t+dt
				(drmt, dqmt) = discs $ t-dt
			in case t of
				0.0->(var (strikept/s) (t+dt) -w)/dt
				_->(var (strikept/s) (t+dt)-var (strikemt/s) (t-dt))/2.0/dt
			
		solution = dwdt/(1.0-y/w*dwdy+0.25*(-0.25-1.0/w+y*y/w/w)*dwdy*dwdy+0.5*d2wdy2)
