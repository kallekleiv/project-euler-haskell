-- Problem 1: Multiples of 3 or 5
-- If we list all the natural numbers below 10 that are multiples
-- of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

multiples :: Int -> Int
multiples x = multiples' (x - 1) 0

multiples' :: Int -> Int -> Int
multiples' 0 sum                    = sum
multiples' x sum | (x `mod` 3) == 0 = multiples' (x - 1) (sum + x)
                 | (x `mod` 5) == 0 = multiples' (x - 1) (sum + x)
                 | otherwise        = multiples' (x - 1)  sum 