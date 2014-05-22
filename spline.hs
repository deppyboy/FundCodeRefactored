import qualified Data.Vector as V
import qualified Data.VectorSpace as VS
import qualified Math.Spline as S
import HassetsMath

--splineinterp :: [Double]->[Double]->Int->(Double->(Double,Double))
splineinterp xs ys deg = f
	where f x | x>=head xs = head ys
		  | x<=last xs = last ys
		  | otherwise = \x->S.evalSpline spline x
		where
			controlpts = V.fromList $ zipWith (\x y->(x,y)) xs ys
			knots = S.mkKnots [0.0, 1.0/spans, 1.0]
			spans = fromIntegral $ deg + length xs
			spline = S.bSpline knots controlpts
			num = fst . f

a = [0.0,0.1..1.0]
b = map sin a
c = splineinterp a b 3
