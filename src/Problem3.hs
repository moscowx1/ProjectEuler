-- Largest prime factor

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

-- answer 6857

module Problem3 where

import Data.Foldable (find)
import Data.Maybe (fromJust)

maxDivCand :: Int -> Int
maxDivCand = floor . sqrt . fromIntegral

divisorCand :: Int -> [Int]
divisorCand n = [2 .. maxDivCand n]

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) (divisorCand n)

maxPrimeFac :: Int -> Maybe Int
maxPrimeFac n =
  find
    (\x -> isPrime x && n `mod` x == 0)
    (reverse $ divisorCand n)

solution :: Int
solution = fromJust $ maxPrimeFac 600851475143

-- (18.44 secs, 10,948,125,712 bytes)

divAll :: Int -> Int -> Int
divAll d 1 = d
divAll d d' =
  if d `mod` d' == 0
    then divAll (d `div` d') d'
    else d

maxP :: (Int, Int, Int) -> Int
maxP (divi, cur, prevP)
  | cur > divi = prevP
  | isPrime cur = maxP (divi `divAll` cur, cur + 1, cur)
  | otherwise = maxP (divi, cur + 1, prevP)

sol2 :: Int -> Int
sol2 n = maxP (n, 1, 1)
-- (0.04 secs, 22,481,984 bytes)