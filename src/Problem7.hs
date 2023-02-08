-- 10001st prime

-- By listing the first six prime numbers: 2, 3,
-- 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

module Problem7 where

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) divisorCand
  where
    divisorCand = [2 .. (floor . sqrt . fromIntegral) n]

solution :: Int
solution = last $ take 10_001 $ filter isPrime [2 ..]