module VolSurf where

import CubicSpline
import qualified Data.Vector.Unboxed as U

--rows of grid are maturities, columns are strikes
data VolSurf a =   VolSurf { 	grid :: [[a]], 
				maturities :: [a],
				strikes :: [a],
				splines ::  [CSpline a]} deriving (Show, Eq)

mkVolSurf :: (Floating a, Ord a, U.Unbox a)=>[[a]]->[a]->[a]->VolSurf a
mkVolSurf ingrid inmats instrikes = VolSurf ingrid inmats instrikes csplines
	where
		csplines = [createSpline instrikes k | k<-ingrid]

volatility :: (Floating a, Ord a, U.Unbox a)=>VolSurf a->a->a->a
volatility (VolSurf _ mats _ csplines) strike mat = evalSpline newspline mat
	where newspline = createSpline mats [evalSpline k strike | k<-csplines]


variance :: (Floating a, Ord a, U.Unbox a)=>VolSurf a->a->a->a
variance vs strike mat = vol*vol*mat
	where vol = volatility vs strike mat

