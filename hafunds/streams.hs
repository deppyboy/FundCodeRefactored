module Streams where

import Data.Time
import Data.List
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Maybe (fromJust)
import Data.Function (on)

data Mapping = Mapping { mappingIndex :: String,
						 weight :: Double } deriving (Eq, Show)

data MarketData = MarketData   { basketIndex :: String,
								 indexStream :: ReturnStream} deriving (Eq, Show)

data ReturnStream = ReturnStream { startDates :: [Day],
								   endDates   :: [Day],
								   returnVals    :: [Double]} deriving (Eq, Show)

dayReturn :: ReturnStream -> Day -> Maybe Double
dayReturn rs dte = do
	x<-elemIndex dte $ endDates rs
	return $ returnVals rs !! x

periodReturn :: ReturnStream -> Day -> Day -> Double
periodReturn  rs start end = totalret $ getSlice rs start end


streamFromNAVs :: [Day] -> [Double] -> ReturnStream
streamFromNAVs dates quotes = ReturnStream (init dates) (tail dates) rets 
	where rets = zipWith (\x y->x/y-1.0) (tail quotes) (init quotes)

streamFromNAVDivs :: [Day] -> [Double] -> [Double] -> ReturnStream
streamFromNAVDivs dates quotes divs = ReturnStream (init dates) (tail dates) convrets
	where 
		rets = zipWith (\x y->x/y-1.0) (tail quotes) (init quotes)
		convrets = zipWith (+) rets $ tail divs

getSlice :: ReturnStream -> Day -> Day -> ReturnStream
getSlice (ReturnStream starts ends rets) begDate endDate = createFromTuple slice
	where
		slice = [(st, ed, ret) | (st, ed, ret) <- zip3 starts ends rets,
								 st >= begDate && ed <= endDate]


overlap :: [ReturnStream] -> [ReturnStream]
overlap a = map (dateReturns oldates) a
	where oldates = overlapDates a

overlapDates :: [ReturnStream] -> [Day]
overlapDates (rs1:rss) = foldl' orderedIntersect (extract rs1) $ map extract rss
	where 
		extract x = (head . startDates) x : endDates x
overlapDates _ = []

dateReturns :: [Day] -> ReturnStream -> ReturnStream
dateReturns datevals rs = createFromTuple $ dateReturns' 1 rs datevals
	where
		dateReturns' accum 
					 rs'@(ReturnStream (st1:st2:sts) (ed:eds) (rt:rts)) 
					 ds@(dt1:dt2:dts) 
					    | dt1<st1 = dateReturns' 1 rs' (dt2:dts)
					    | dt2 == ed && dt1 == st1 = (dt1, dt2, (1+rt)*accum-1) : 
					 	        dateReturns' 1 (ReturnStream (st2:sts) eds rts) (dt2 : dts)
					    | dt2 > ed = dateReturns' (accum*(1+rt)) (ReturnStream (st1:sts) eds rts) ds
						| otherwise = []
		dateReturns' _ _ _ = []

createFromTuple :: [(Day, Day, Double)] -> ReturnStream
createFromTuple a = ReturnStream sts eds rets
	where (sts, eds, rets) = unzip3 a

--just an O(n) function for intersection with two ordered lists.
orderedIntersect :: Ord a => [a] -> [a] -> [a]
orderedIntersect (x:xs) (y:ys) | x==y = x : orderedIntersect xs ys
							   | x>y = orderedIntersect (x:xs) ys
							   | otherwise = orderedIntersect xs (y:ys)
orderedIntersect _ _ = []

createProjected :: [MarketData] -> Day -> Day -> [Mapping] -> Double
createProjected basket st ed (m:ms) = weight m * periodReturn (indexStream b) st ed + createProjected basket st ed ms
	where
		b = basketGetUnsafe basket (mappingIndex m)
createProjected _ _ _ _ = 0.0

basketGetUnsafe :: [MarketData] -> String -> MarketData
basketGetUnsafe bs idx = fromJust $ basketGet bs idx

basketGet :: [MarketData] -> String -> Maybe MarketData
basketGet (b:bs) idx | idx == basketIndex b = Just b
					 | otherwise = basketGet bs idx
basketGet _ _ = Nothing

--Operations on ReturnStreams.
cov :: ReturnStream -> ReturnStream -> Double
cov = olArgs (\x y->adjmean $ (x <-> mean x) * (y <-> mean y))

var :: ReturnStream -> Double
var a = cov a a

stdev :: ReturnStream -> Double
stdev = sqrt . var

beta :: ReturnStream -> ReturnStream -> Double
beta = olArgs $ \a b->cov a b / var b

correl :: ReturnStream -> ReturnStream -> Double
correl = olArgs $ \x y->cov x y / stdev x / stdev y

te :: ReturnStream -> ReturnStream -> Double
te = olArgs $ \a b->stdev $ a-b

alpha :: ReturnStream -> ReturnStream -> Double
alpha = olArgs ((-) `on` totalret)

totalret :: ReturnStream -> Double
totalret x = foldl1' (*) (returnVals $ x <+> 1)-1.0

olArgs :: (ReturnStream -> ReturnStream -> t) -> ReturnStream -> ReturnStream -> t
olArgs f x y = f olx oly
	where [olx, oly] = overlap [x,y]

binOp :: (Double->Double->Double) -> ReturnStream -> ReturnStream -> ReturnStream
binOp f a b = ReturnStream (startDates ol1) (endDates ol1) $ zipWith f rets1 rets2 
	where
		[ol1, ol2] = overlap [a,b]
		rets1 = returnVals ol1
		rets2 = returnVals ol2

max :: ReturnStream -> Double
max = maximum . returnVals

min :: ReturnStream -> Double
min = minimum . returnVals

instance Num ReturnStream where
	(+) = binOp (+)
	(-) = binOp (-)
	(*) = binOp (*)
	negate = elementWise negate
	signum = elementWise signum
	abs = elementWise abs
	fromInteger _ = ReturnStream [] [] []

infixl 6 <+>
(<+>) :: ReturnStream -> Double -> ReturnStream
a <+> b = elementWise (+b) a

infixl 6 <->
(<->) :: ReturnStream -> Double -> ReturnStream
a <-> b = elementWise (+negate b) a

infixl 7 <*>
(<*>) :: ReturnStream -> Double -> ReturnStream
a <*> b = elementWise (*b) a

infixl 7 </>
(</>) :: ReturnStream -> Double -> ReturnStream
a </> b = elementWise (/b) a

mean :: ReturnStream -> Double
mean x = sumReturns x / fromIntegral (length $ returnVals x)

adjmean :: ReturnStream -> Double
adjmean x = sumReturns x / (fromIntegral (length $ returnVals x)-1)

sumReturns :: ReturnStream -> Double
sumReturns = sum . returnVals

elementWise :: (Double -> Double) -> ReturnStream -> ReturnStream
elementWise f (ReturnStream sts eds rets) = ReturnStream sts eds $ map f rets

logconvert :: ReturnStream -> ReturnStream
logconvert = elementWise (\x->log $ 1+x)

weeklyFreq :: ReturnStream -> Int -> ReturnStream
weeklyFreq rs dayval = createFromTuple $ weeklyFreq' rs dayval
	where 
	weeklyFreq' (ReturnStream   (st1:st2:sts)
				                (ed1:eds) 
						        (rt1:rt2:rts)) day
			| weekday == day = (st1, ed1, rt1) : 
					weeklyFreq' (ReturnStream (st2:sts) eds (rt2:rts)) day
			| otherwise = weeklyFreq' (ReturnStream (st1:sts) eds (((1+rt1)*(1+rt2)-1):rts)) day
				where
					(_, _, weekday) = toWeekDate ed1
	weeklyFreq' (ReturnStream   (st:_)
				                (ed:_) 
						        (rt:_)) day
			| weekday == day = [(st, ed, rt)]
			| otherwise = []
				where
					(_, _, weekday) = toWeekDate ed
	weeklyFreq' _ _ = []

monthlyFreq :: ReturnStream -> ReturnStream
monthlyFreq rs = createFromTuple $ monthlyFreq' rs
	where 
	monthlyFreq' (ReturnStream   (st1:sts) 
				                 (ed1:ed2:eds) 
						         (rt1:rt2:rts))
			| monthval ed1 /= monthval ed2 = (st1, ed1, rt1) : monthlyFreq' (ReturnStream sts (ed2:eds) (rt2:rts)) 
			| otherwise = monthlyFreq' (ReturnStream (st1:sts) (ed2:eds) (((1+rt1)*(1+rt2)-1):rts)) 
				where
					monthval x = num 
						where (_, num, _) = toGregorian x
	monthlyFreq' (ReturnStream   (st:_) 
				                 (ed:_) 
						         (rt:_)) = [(st, ed, rt)]
	monthlyFreq' _ = []