module HassetsMath where

import CubicSpline
import qualified Data.Vector as V


interpolate1d :: (Fractional a, Ord a) => [a]->[a]->a->a
interpolate1d (x1:x2:xs) (y1:y2:ys) xval | xval<=x1 = y1
					 					 | xval>=x2 = interpolate1d (x2:xs) (y2:ys) xval
					 					 | otherwise = ((xval-x1)*y2+(x2-xval)*y1)/(x2-x1)
interpolate1d _ (y1:_) _ = y1
interpolate1d _ _ _ = error "Trying to interpolate on empty yield curve."

interpolate1dcubic :: (Fractional a, Ord a) => [a]->[a]->a->a
interpolate1dcubic xs ys = evalSpline (createSpline xs ys)

interpolate2d :: (Fractional a, Ord a) => [[a]]->[a]->[a]->a->a->a
interpolate2d grid xs ys xval yval = interpolate1d xs [interpolate1d ys row yval | row<-grid] xval

interpolate2dcubic :: (Fractional a, Ord a) => [[a]]->[a]->[a]->a->a->a
interpolate2dcubic grid xs ys xval yval = interpolate1dcubic xs [interpolate1dcubic ys row yval | row<-grid] xval

simpint :: (Enum a, Floating a)=>(a->a)->a->a->Int->a
simpint f a b intervals = 
	dx * ((V.head fy + V.last fy)/6.0 + V.sum(V.tail $ V.init fy)/3.0 + V.sum(V.tail fx)*2.0/3.0)
		where
                  dx = (b-a)/fromIntegral intervals
                  fy = V.map f $ V.enumFromStepN a dx (intervals+1)
                  fx = V.map f $ V.enumFromStepN (a-0.5*dx) dx (intervals+1)

newtonmethod :: (Enum a, Floating a, Ord a)=>(a->a)->a->a->a->a->Maybe a
newtonmethod f target guess tol perturb | abs (target - f guess) < tol = Just guess
										| dydx == 0.0 = Nothing
										| otherwise = newtonmethod f target newguess tol perturb
						where
							dydx = (f (guess+perturb) - f (guess-perturb))/2.0/perturb
							newguess = guess-(f guess-target)/dydx