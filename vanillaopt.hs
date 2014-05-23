module VanillaOpt where

import Equity
import Data.Complex
import HassetsMath
import YC
import EqProcess
import qualified Equity as EQ
import qualified Data.Vector.Unboxed as U


data OptionType = Put | Call deriving (Eq, Show, Ord)

data VanillaOption a = VanillaOption { 
					strike :: a, 
					maturity :: a, 
					notional :: a, 
					pc :: OptionType }

vanillaopt :: (CharFunc a, RealFloat b, Enum b, U.Unbox b)=>VanillaOption b->Equity b (a b)->b
vanillaopt (VanillaOption strike t notional pc) eq | pc == Call = lvl*q*p1-strike*disc1*p2
						   | pc == Put  = strike*disc1*(1.0-p2)-lvl*q*(1.0-p1)
	where
		cf = charfunc eq t
		i = 0.0:+1.0
		cfi = cf (-i)
		k = log strike:+0.0
		intfunc1 = \u->realPart $ exp(-i*(u:+0.0)*k)*cf ((u:+0.0)-i)/i/(u:+0.0)/cfi
		intfunc2 = \u->realPart $ exp(-i*(u:+0.0)*k)*cf (u:+0.0)/i/(u:+0.0)
		p1 = 0.5+(simpint intfunc1 1e-8 50.0 1000)/pi
		p2 = 0.5+(simpint intfunc2 1e-8 50.0 1000)/pi
		disc1 = disc (EQ.rf eq) t
		q = disc (EQ.dividend eq) t
		lvl = level eq
