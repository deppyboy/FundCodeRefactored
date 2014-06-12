module VectorOps where

import qualified Data.Vector as V

infixl 7 <*>
(<*>) :: (Num a)=>V.Vector a->V.Vector a->V.Vector a
(<*>) = V.zipWith (*)

infixl 7 </>
(</>) :: (Fractional a)=>V.Vector a->V.Vector a->V.Vector a
(</>) = V.zipWith (/)

infixl 6 <+>
(<+>) :: (Num a)=>V.Vector a->V.Vector a->V.Vector a
(<+>) = V.zipWith (+)

infixl 6 <->
(<->) :: (Num a)=>V.Vector a->V.Vector a->V.Vector a
(<->) = V.zipWith (-)

infixl 7 <!*>
(<!*>) :: (Num a)=>a->V.Vector a->V.Vector a
(<!*>) x = V.map (*x)

infixl 7 <!/>
(<!/>) :: (Fractional a)=>a->V.Vector a->V.Vector a
(<!/>) x = V.map (\y->x/y)

infixl 6 <!+>
(<!+>) :: (Num a)=>a->V.Vector a->V.Vector a
(<!+>) x = V.map (+x)

infixl 6 <!->
(<!->) :: (Num a)=>a->V.Vector a->V.Vector a
(<!->) x = V.map (\y->x-y)

infixl 7 <*!>
(<*!>) :: (Num a)=>V.Vector a->a->V.Vector a
(<*!>) = flip (<!*>)

infixl 7 </!>
(</!>) :: (Fractional a)=>V.Vector a->a->V.Vector a
(</!>) x y = V.map (/y) x

infixl 6 <+!>
(<+!>) :: (Num a)=>V.Vector a->a->V.Vector a
(<+!>) = flip (<!+>)

infixl 6 <-!>
(<-!>) :: (Num a)=>V.Vector a->a->V.Vector a
(<-!>) x y = V.map (\a->a-y) x

sqrtV :: Floating a=>V.Vector a->V.Vector a
sqrtV = V.map sqrt

expV :: Floating a=>V.Vector a->V.Vector a
expV = V.map exp