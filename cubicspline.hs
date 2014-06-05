module CubicSpline where

import qualified Data.Vector as V

-- | The 'CSpline' object holds three lists: one for the knots, one for the control points, and one for the moments.
data CSpline a = CSpline { knots   :: V.Vector a,
		 	   			   cps     :: V.Vector a,
		 	   			   moments :: V.Vector a } deriving (Show, Eq)

-- | The 'createSpline' function creates a spline from a list of knots and control points.
createSpline :: (Fractional a, Ord a)=>[a]->[a]->CSpline a
createSpline knotvals controls = CSpline knotvals' controls' $ tdmaSolver mu (V.replicate (V.length d) 2.0) lambda d
	where
		knotvals' = V.fromList knotvals
		controls' = V.fromList controls
		h = V.zipWith (-) (V.tail knotvals') (V.init knotvals')
		lambda = begZero $ V.zipWith (\x y->x/(x+y)) (V.tail h) (V.init h)
		mu = endZero $ V.map (\x->1.0-x) lambda
		dj hj hj1 yj1 yj yjm1= 6.0/(hj+hj1)*((yj1-yj)/hj1-(yj-yjm1)/hj)
		d = endZero $ begZero $ V.zipWith5 dj (V.init h) (V.tail h) 
				(V.tail $ V.tail controls') (V.tail $ V.init controls') 
				(V.init $ V.init controls')

-- | Evaluates a CSpline object.
evalSpline :: (Fractional a, Ord a)=>CSpline a->a->a
evalSpline (CSpline ks ys ms) x | x<=V.head ks = V.head ys
			        		    | x>=V.last ks = V.last ys
			        		    | otherwise = splineCalc k1 k2 y1 y2 m1 m2
									where
										(kstart, kend) = V.partition (<x) ks
										(k1, k2) = (V.last kstart, V.head kend)
										getslice = V.slice (V.length kstart-1) 2
										getvals y = (y V.! 0, y V.! 1)
										(y1, y2) = getvals $ getslice ys
										(m1, m2) = getvals $ getslice ms
										splineCalc k1' k2' y1' y2' m1' m2' = y1'+beta*term+gamma*term^2+
											delta*term^3
											where
												gamma = m1'/2.0
												beta = (y2'-y1')/h-(2*m1'+m2')*h/6.0
												delta = (m2'-m1')/6.0/h
												term = x-k1'
												h = k2'-k1'
			

-- | A simple function to solve tridiagonal matricies.
tdmaSolver :: (Fractional a, Ord a)=>V.Vector a->V.Vector a->V.Vector a->V.Vector a->V.Vector a
tdmaSolver a b c d = V.reverse $ solution $ V.zip (begZero $ V.reverse cvals) (V.reverse dvals)
	where
		cvals = scanlt0 (\priorc (a1,b1,c1)->c1/(b1-priorc*a1)) $ 
					V.zip3 (begZero $ V.init a) (V.init b) c
		dvals = scanlt0 (\priord (a1,b1,c1,d1)->(d1-a1*priord)/(b1-a1*c1)) $ 
					V.zip4 (begZero a) b (begZero cvals) d
		solution = scanlt0 (\priorx (cn, dn)->dn-cn*priorx)
		scanlt0 f = V.tail . V.scanl f 0.0

begZero :: (Fractional a, Ord a)=>V.Vector a->V.Vector a
begZero = V.cons 0.0

endZero :: (Fractional a, Ord a)=>V.Vector a->V.Vector a
endZero = \x->V.snoc x 0.0