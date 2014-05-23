module VolSurf where

import HassetsMath
import qualified Data.Vector.Unboxed as U

--TODO: Redo with splines as internals!

data VolSurf a =   VolSurf { 	grid :: [[a]],
				maturities :: [a],
				strikes :: [a] } deriving (Show, Eq)

volatility :: (Floating a, Ord a, U.Unbox a)=>VolSurf a->a->a->a
volatility (VolSurf grid maturities strikes) strike mat = interpolate2dcubic grid strikes maturities strike mat

variance :: (Floating a, Ord a, U.Unbox a)=>VolSurf a->a->a->a
variance vs strike mat = vol*vol*mat
	where vol = volatility vs strike mat
