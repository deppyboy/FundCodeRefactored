module EqProcess where

import Data.Complex
import YC
import VolSurf
import RNG
import Control.Monad.State
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V


--Just a few helpers
(<+>) :: (U.Unbox a, Num a)=>U.Vector a->U.Vector a->U.Vector a
(<+>) = U.zipWith (+)
(<->) :: (U.Unbox a, Num a)=>U.Vector a->U.Vector a->U.Vector a
(<->) = U.zipWith (-)
(<*>) :: (U.Unbox a, Num a)=>U.Vector a->U.Vector a->U.Vector a
(<*>) = U.zipWith (*)
(</>) :: (U.Unbox a, RealFloat a)=>U.Vector a->U.Vector a->U.Vector a
(</>) = U.zipWith (/)
vlog :: (U.Unbox a, Floating a)=>U.Vector a->U.Vector a
vlog = U.map log
vexp :: (U.Unbox a, Floating a)=>U.Vector a->U.Vector a
vexp = U.map exp

data Lognormal a = Lognormal { lnvolatility :: a } deriving (Show, Eq)

data Heston a = Heston { v0 :: U.Vector a,
			 vf :: a, 
			 kappa :: a, 
			 volvol :: a, 
			 rho :: a } deriving (Show, Eq)

data Dupire a = Dupire { vols :: VolSurf a,
			 s0   :: a} deriving (Show, Eq)

data MCState a = MCState { model :: a Double, 
			   rngstate :: PrefetchRands, 
			   lvlstate :: U.Vector Double }

class Discretize a where
	evolve ::  YieldCurve Double->
		   YieldCurve Double->
		   Double->            
		   Double->
		   Int->
		   State (MCState a) (U.Vector Double)

--	genpath :: YieldCurve Double->
--		   YieldCurve Double->
--		   Double->            
--		   Double->            
--		   Int->
--		   Int->               
--		   State (MCState a) (V.Vector (U.Vector Double))
--	genpath rf div t1 t2 intervals = do
--		let
--			genpath' lvls start 1 = do
--				newlevel <- evolve rf div start t2
--				return $ V.snoc lvls newlevel
--			genpath' lvls start n = do
--				let dt = (t2-start)/(fromIntegral n)
--				newlvl <- evolve rf div start (start+dt)
--				solution <- genpath' (V.snoc lvls newlvl) (start+dt) (n-1)
--				return solution
--		result <- genpath' V.empty t1 intervals
--		return result
				

randWrapperMulti x n = state $ f
	where 
		f (MCState a1 myrng a3) = (U.fromList randvals, MCState a1 newrng a3)
			where (randvals, newrng) = fetchrands myrng x n


instance Discretize Lognormal where
	evolve rf div t1 t2 trials = do
		rand<-randWrapperMulti Normal trials
		internal<-get
		let
			(model, rng, lvl) = unwrap internal
			t = t2-t1
			r = forward rf t1 t2 - forward div t1 t2
			vol = lnvolatility model
			rate = (*) t (r-vol*vol*t/2.0)
			tvol = vol*sqrt t
			growth = vexp $ U.map (+rate) $ U.map (*tvol) rand
			newlvl = lvl <*> growth
		put $ MCState model rng newlvl
		return $ newlvl

instance Discretize Dupire where
	evolve rf div t1 t2 trials = do
		rand<-randWrapperMulti Normal trials
		internal<-get
		let 
			(model, rng, lvl) = unwrap internal
			t = t2-t1
			r = forward rf t1 t2 - forward div t1 t2
			lvol = U.map (\x->localvol model rf div x t1) lvl
			rate = U.map (*t) $ U.map (+r) $ (U.map ((*(-t)).(/2.0))) (lvol <*> lvol)
			ret = rate <+> U.map (*sqrt t) (rand<*>lvol)
			newlvl = lvl <*> vexp ret
		put $ MCState model rng newlvl
		return $ newlvl

instance Discretize Heston where
	evolve rf div t1 t2 trials = do
		x<-randWrapperMulti Normal trials
		y<-randWrapperMulti Normal trials
		internal<-get
		let 
			(model, rng, lvl) = unwrap internal
			(vinit, vfinal, kap, sigma, correl) = (\(Heston a b c d e)->(a,b,c,d,e)) model
			z = (U.map (*correl) x) <+> (U.map (\j->sqrt (1-correl*correl)*j) y)
			r = forward rf t1 t2 - forward div t1 t2
			t = t2-t1
			growth = U.zipWith (\a b->r-a/2.0+b*sqrt (a * t)) vinit x
			newlvl = lvl <*> (vexp growth)
			f = \vi corrrand->(sqrt vi+sigma/2.0*(sqrt t)*corrrand)^2-kap*(vi-vfinal)*t-sigma*sigma*t/4.0
			newv = U.zipWith f vinit z
			flipv = U.map (\k->if k>0.0 then k else -k) newv
		put (MCState (Heston flipv vfinal kap sigma correl) rng newlvl)
		return newlvl

unwrap :: MCState t-> (t Double, PrefetchRands, U.Vector Double)
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
