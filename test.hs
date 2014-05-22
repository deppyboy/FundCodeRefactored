import Equity
import VolSurf
import YC
import EqProcess
import VanillaOpt

mygrid = [[0.2,0.2,0.2],
          [0.3,0.3,0.3],
	  [0.4,0.4,0.4]]
mymats = [1.0, 2.0, 3.0]
mystrikes = [0.5, 1.0, 1.5]

vs = VolSurf mygrid mymats mystrikes
myyc = basiccurve 0.05
mydiv = basiccurve 0.02
ln = Lognormal 0.2 "SPX"
h = Heston 0.04 0.04 1.0 0.2 (-0.7) "SPX"
x = Equity "SPX" 100.0 myyc mydiv vs h
v = vanillaopt (VanillaOption 100.0 1.0 1.0 Put) x
