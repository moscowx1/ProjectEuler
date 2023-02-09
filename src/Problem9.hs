-- Special Pythagorean triplet

-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

module Problem9 () where

solve maxi =
  [ a * b * c | a <- [1 .. maxi], b <- [a .. maxi], let c = maxi - a - b, a * a + b * b == c * c
  ]

solution :: [Int]
solution = solve 1000