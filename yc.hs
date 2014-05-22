module YC where

import HassetsMath
import CubicSpline
import qualified Data.Vector.Unboxed as U

data YieldCurve a = LinearPar	 {  lpmats  :: [a], 
                 	            linearrates :: [a] }   |
		    CubicParLinearLogDisc
				 {  ldmats :: [a],
				    logdiscs :: [a] }      | 
		    CubicParCubicSpot
				 {  cspline :: CSpline a } |
		    FlatCurve { rate :: a }
			 deriving (Show, Eq)


--takes a par curve uses lots of linear interpolation
stripcurvelinearpar :: (RealFloat a, Enum a, Ord a)=>[a]->[a]->YieldCurve a
stripcurvelinearpar mats rates = LinearPar times $ recursivestrip times interprates 0.0
	where
		times = [0.5,1.0..40]
		interprates = map interp times
		interp = interpolate1d mats rates
		recursivestrip [] [] _ = []
		recursivestrip (t:ts) (x:xs) cumdisc = (-log(disc)/t):recursivestrip ts xs (disc+cumdisc)
			where disc = (1.0-x/2.0*cumdisc)/(1.0+x/2.0)

--takes a par curve and uses a cubic spline on pars and linear on the log-discs
stripcurvecubicparlogdisc :: (RealFloat a, Enum a, Ord a, U.Unbox a)=>[a]->[a]->YieldCurve a
stripcurvecubicparlogdisc mats rates = CubicParLinearLogDisc times $ recursivestrip times interprates 0.0
	where
		times = [0.5,1.0..40]
		interprates = map (evalSpline (createSpline mats rates)) times
		recursivestrip [] [] _ = []
		recursivestrip (t:ts) (x:xs) cumdisc = log disc : recursivestrip ts xs (disc+cumdisc)
			where disc = (1.0-x/2.0*cumdisc)/(1.0+x/2.0)

--takes a par curve and uses a cubic spline on pars cubic spline on rates
stripcurvecubiccubic :: (RealFloat a, Enum a, Ord a, U.Unbox a)=>[a]->[a]->YieldCurve a
stripcurvecubiccubic mats rates = CubicParCubicSpot $ createSpline times $ recursivestrip times interprates 0.0
	where
		times = [0.5,1.0..40]
		interprates = map (evalSpline (createSpline mats rates)) times
		recursivestrip [] [] _ = []
		recursivestrip (t:ts) (x:xs) cumdisc = (-log(disc)/t):recursivestrip ts xs (disc+cumdisc)
			where disc = (1.0-x/2.0*cumdisc)/(1.0+x/2.0)


--calculates discount factors
disc :: (RealFloat a, Ord a, U.Unbox a)=>YieldCurve a->a->a
disc (LinearPar mats rates) t = exp $ (*) (-t) $ interpolate1d mats rates t
disc (CubicParLinearLogDisc mats logdiscs) t = exp $ interpolate1d mats logdiscs t
disc (CubicParCubicSpot cs) t = exp $ (*) (-t) $ evalSpline cs t
disc (FlatCurve r) t = exp $ -r*t

--calculates forward rates
forward :: (RealFloat a, Ord a, U.Unbox a)=>YieldCurve a->a->a->a
forward yc t1 t2 = -log(d2 / d1)/(t2-t1)
	where
		d1 = disc yc t1
		d2 = disc yc t2

--calculates par rates
parswap :: (RealFloat a, Enum a, Ord a, U.Unbox a)=>YieldCurve a->a->a
parswap yc tmat = (1.0-(last discs)) / sum (discs) *2.0
	where discs = map (disc yc) [0.5,1.0..tmat]

--just for testing/fun
basiccurve :: (RealFloat a, U.Unbox a)=>a->YieldCurve a
basiccurve rate = LinearPar [0.0,40.0] [rate, rate]
