-- Smallest multiple

-- 2520 is the smallest number that can
-- be divided by each of the numbers from 1 to
-- 10 without any remainder.
-- What is the smallest positive number
-- that is evenly divisible by all of
-- the numbers from 1 to 20?

module Problem5 where

import Data.List (groupBy)

-- ans 232792560
divs :: [Integer]
divs = [2 .. 20]

isMn :: Integer -> Bool
isMn n = all (\x -> n `mod` x == 0) divs

solution :: Integer
solution = head $ take 1 $ filter isMn [1 ..]

-- (208.01 secs, 112,076,358,360 bytes)

isPrime :: Integer -> Bool
isPrime n = all (\x -> n `mod` x /= 0) divisorCand
  where
    divisorCand = [2 .. (floor . sqrt . fromIntegral) n]

minProbDiv :: Integer
minProbDiv = foldl (*) 1 $ filter isPrime divs

solution2 :: Integer
solution2 = head $ take 1 $ filter isMn [minProbDiv ..]

-- (205.14 secs, 107,406,566,200 bytes)

counts :: Integer -> Integer -> Integer
counts x y
  | x `mod` y == 0 = 1 + counts (x `div` y) y
  | otherwise = 0

solution3 :: Integer
solution3 = product $ map (uncurry (^)) primeMaxC
  where
    primeMaxC = map (\xs -> (fst $ head xs, maximum $ map snd xs)) groupPrimeC
    groupPrimeC = groupBy (\x y -> fst x == fst y) primeCount
    primeCount = [(p, c) | p <- primes, n <- nums, let c = counts n p, c > 0]
    primes = filter isPrime nums
    nums = [2 .. 20]

-- (0.00 secs, 645,504 bytes)
