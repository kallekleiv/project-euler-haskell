-- Problem 3: Largest Prime Factor
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600 851 475 143?

highestprimenumber :: Int -> Int
highestprimenumber num = if (isEven num) then 2 else findhighestfactor (divisor num) (divisor num)

findhighestfactor :: Int -> Int -> Int
findhighestfactor x y = if factor y x then y else findhighestfactor x (y - 2)

-- Checks whether a number is prime or not
--prime :: Int -> Bool
--prime x = primefactors x 1 []

--primefactors :: Int -> Int -> [Int] -> Bool
--primefactors _ _ (_:_:_) = False
--primefactors x i (a:b:[]) = (i > x) --No clause for empty list
--primefactors x i [] = if (factor i x)
--                      then primefactors x (i + 1) [i]
--                      else primefactors x (i + 1) []
--primefactors x i xs = if (factor i x)
--                      then primefactors x (i + 1) (xs ++ [i])
 --                     else primefactors x (i + 1) xs

-- Checks whether x is a factor in y
factor :: Int -> Int -> Bool
factor x y = y `mod` x == 0

divisor :: Int -> Int 
divisor num = ((num + 1) `div` 2 - 1)

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0