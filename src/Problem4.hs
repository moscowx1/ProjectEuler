-- Largest palindrome product

-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two
-- 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome
-- made from the product of two 3-digit numbers

module Problem4 where

isPal :: Int -> Bool
isPal n = show n == reverse (show n)

solution =
  maximum $
    filter isPal $
      concatMap (\x -> (* x) <$> filter (x >) n) n
  where
    n = [999, 998 .. 900]

-- ans 906609