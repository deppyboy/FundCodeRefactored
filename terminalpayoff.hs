module TerminalPayoff where

import EqProcess
import CubicSpline
import qualified Data.Vector as V
import VectorOps

data Method = Implicit | CrankNicholson

pdeSolve dp rf div t f Implicit lbound ubound partitions timeSteps = 
		where
			mesh = V.enumFromStepN lbound step partitions
			step = (ubound-lbound)/fromIntegral partitions
			xd = mesh </!> step
			ln = V.length mesh
			evolveToEnd 0.0 stepSize pdeMesh = pdeMesh
			evolveToEnd remDist stepSize pdeMesh = evolveToEnd (remDist-stepSize) (if stepSize>remDist then remDist else stepSize) newMesh
				where
					alpha = volsimp <*> volsimp <*> xd <*> xd <*!> step </!> 2.0 
					beta = (r-q) <!*> xd <*!> (step/2.0)
					r = forward rf (remDist-stepSize) remDist
					q = forward div (remDist-stepSize) remDist
					A = beta <-> alpha
					B = 1.0+r*stepSize<!+>2.0<!*>alpha
					C = (-1.0)<!*>alpha<->beta
					down = 2.0*pdeMesh V.! 1-pdeMesh V.! 2
					up = 2.0*pdeMesh V.! (V.)
					updV = V.cons down $ V.snoc (init $ tail pdeMesh) up
					volsimp = V.map (\x->localvol dp rf div x remDist) mesh
					newMesh = tdmaSolver (V.tail A) B (V.init C) pdeMesh