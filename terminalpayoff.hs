module TerminalPayoff where

import EqProcess
import qualified Data.Vector as V
import VectorOps
import Control.Monad.State

data Method = Implicit | CrankNicholson

--pdeSolve :: (Integral a1, RealFloat a)=>Dupire a->YieldCurve a->YieldCurve a->a->(a->a)->Method->a->a->Int->a1->a
--pdeSolve dp rf dv t f Implicit lbound ubound partitions timeSteps = evolveToEnd t (t/fromIntegral timeSteps) apmesh
--		where
--			mesh = V.enumFromStepN lbound step partitions
--			apmesh = V.map f mesh
--			step = (ubound-lbound)/fromIntegral partitions
--			xd = mesh </!> step
--			evolveToEnd 0.0 stepSize pdeMesh = pdeMesh
--			evolveToEnd remDist stepSize pdeMesh = evolveToEnd (remDist-stepSize) (if stepSize>remDist then remDist else stepSize) newMesh
--				where
--					alpha = volsimp <*> volsimp <*> xd <*> xd <*!> step </!> 2.0 
--					beta = (r-q) <!*> xd <*!> (step/2.0)
--					r = forward rf (remDist-stepSize) remDist
--					q = forward dv (remDist-stepSize) remDist
--					a = beta <-> alpha
--					b = 1.0+r*stepSize<!+>2.0<!*>alpha
--					c = (-1.0)<!*>alpha<->beta
--					down = 2.0*pdeMesh V.! 1-pdeMesh V.! 2
--					up = 2.0*V.last pdeMesh - V.last (V.init pdeMesh)
--					updV = V.cons down $ V.snoc (V.init $ V.tail pdeMesh) up
--					volsimp = V.map (\x->localvol dp rf dv x remDist) mesh
--					newMesh = tdmaSolver (V.tail a) b (V.init c) updV


mcPrice _ _ _ _ _ _ 0 = 0.0
mcPrice payoffFunc rf dv t intervals trials rng = runState 
	where
		(val, st) = runState (genpath rf dv 0.0 t intervals) rng