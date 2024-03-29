module RNG where

import System.Random.Mersenne.Pure64
import Data.Word

data Distribution = Normal | Uniform deriving (Show, Eq)

data PrefetchRands = PrefetchRands { fetched :: [Double], gen :: PureMT } deriving (Show)


boxMuller :: (Floating a)=>a->a->(a,a)
boxMuller r1 r2 = (f cos, f sin)
	where f a = sqrt (-2.0*log r1) * a (2.0*pi*r2)


fetchrand :: PrefetchRands->Distribution->(Double, PrefetchRands)
fetchrand (PrefetchRands xs rng) Normal | null xs = (bm1, PrefetchRands [bm2] gen2)
                                        | otherwise = (head xs, PrefetchRands (tail xs) rng)
                                             where
						(rand1, gen1) = randomDouble rng
						(rand2, gen2) = randomDouble gen1
						(bm1, bm2) = boxMuller rand1 rand2

fetchrand (PrefetchRands _ rng) Uniform = (rand, PrefetchRands [] newrng)
			where (rand, newrng) = randomDouble rng

fetchrands :: (Eq a, Num a)=>PrefetchRands->Distribution->a->([Double], PrefetchRands)
fetchrands (PrefetchRands myfetch mygen) mydist = fetchaccum [] (PrefetchRands myfetch mygen)
		where
			fetchaccum accum pf 0 = (accum, pf)
			fetchaccum accum pf x = fetchaccum (newnorm:accum) newpfr (x-1)
				where (newnorm, newpfr) = fetchrand pf mydist
