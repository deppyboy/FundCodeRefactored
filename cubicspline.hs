module CubicSpline where

import qualified Data.Vector.Unboxed as U



-- | The 'CSpline' object holds three lists: one for the knots, one for the control points, and one for the moments.
data CSpline a = CSpline { knots   :: U.Vector a,
		 	   cps     :: U.Vector a,
		 	   moments :: U.Vector a } deriving (Show, Eq)

-- | The 'createSpline' function creates a spline from a list of knots and control points.
createSpline :: (U.Unbox a, Fractional a, Ord a)=>[a]->[a]->CSpline a
createSpline knotvals controls = CSpline knotvals' controls' $ tdmaSolver mu (U.replicate (U.length d) 2.0) lambda d
	where
		knotvals' = U.fromList knotvals
		controls' = U.fromList controls
		h = U.zipWith (-) (U.tail knotvals') (U.init knotvals')
		lambda = U.cons 0.0 $ U.zipWith (\x y->x/(x+y)) (U.tail h) (U.init h)
		mu = U.snoc (U.map (\x->1.0-x) lambda) 0.0
		dj hj hj1 yj1 yj yjm1= 6.0/(hj+hj1)*((yj1-yj)/hj1-(yj-yjm1)/hj)
		d = U.cons 0.0 $ U.zipWith5 dj (U.init h) (U.tail h) 
				(U.tail $ U.tail controls') (U.tail $ U.init controls') 
				(U.init $ U.init controls') U.++ U.singleton 0.0

-- | Evaluates a CSpline object.
evalSpline :: (U.Unbox a, Fractional a, Ord a)=>CSpline a->a->a
evalSpline (CSpline ks ys ms) x | x<=U.head ks = U.head ys
				| x>=U.last ks = U.last ys
				| otherwise = splineCalc k1 k2 y1 y2 m1 m2
					where
						(kstart, kend) = U.partition (<x) ks
						(k1, k2) = (U.last kstart, U.head kend)
						getslice = U.slice (U.length kstart-1) 2
						getvals y = (y U.! 0, y U.! 1)
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
tdmaSolver :: (U.Unbox a, Fractional a, Ord a)=>U.Vector a->U.Vector a->U.Vector a->U.Vector a->U.Vector a
tdmaSolver a b c d = U.reverse $ solution $ U.zip (U.cons 0.0 $ U.reverse cvals) (U.reverse dvals)
	where
		cvals = scanlt0 (\priorc (a1,b1,c1)->c1/(b1-priorc*a1)) $ 
					U.zip3 (U.cons 0.0 $ U.init a) (U.init b) c
		dvals = scanlt0 (\priord (a1,b1,c1,d1)->(d1-a1*priord)/(b1-a1*c1)) $ 
					U.zip4 (U.cons 0.0 a) b (U.cons 0.0 cvals) d
		solution = scanlt0 (\priorx (cn, dn)->dn-cn*priorx)
		scanlt0 f = U.tail . U.scanl f 0.0
