-- Sum square difference

-- Find the difference between the sum of the squares of
-- the first one hundred natural numbers and the square of the sum.

module Problem6 where

-- ans 25164150

solution :: Int
solution = squreSum - sumSquare
  where
    squreSum = sum nums ^ 2
    sumSquare = sum $ map (^ 2) nums
    nums = [1 .. 100]