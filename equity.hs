module Equity where

import YC
import VolSurf
import EqProcess
import Data.Complex

data Equity a b = Equity { 	
			index :: String,
			level :: a,
			rf :: YieldCurve a,
			dividend :: YieldCurve a,
			volsurf :: VolSurf a,
			process :: b} deriving (Show, Eq)

forwardrate :: RealFloat a=>Equity a b->a->a->a
forwardrate (Equity _ _ r q _ _) t1 t2 = forward r t1 t2-forward q t1 t2

charfunc :: (CharFunc a, RealFloat b)=>Equity b (a b)->b->Complex b->Complex b
charfunc eqprocess t = \x->cf x * exp (i*x*(lns0+g*tc))
	where
			i = 0.0:+1.0
			tc = t:+0.0
			lns0 = log (level eqprocess):+0.0
			g = forwardrate eqprocess 0.0 t:+0.0
			cf = charfuncfactory (process eqprocess) t

