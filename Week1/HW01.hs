{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n - (n `div` 10 * 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n 
    | n > 0     = lastDigit n : (toRevDigits $ dropLastDigit n)
    | otherwise = [] 

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = doubleEveryOtherRec l 1
    where doubleEveryOtherRec [] _ = []
          doubleEveryOtherRec (x:xs) f = (f * x) : doubleEveryOtherRec (xs) f2
              where f2 = if f == 1 then 2 else 1

doubleEveryOther2 :: [Integer] -> [Integer]
doubleEveryOther2 (x1:x2:xs) = x1 : x2*2 : doubleEveryOther2 xs
doubleEveryOther2 (x1:_) = [x1]
doubleEveryOther2 [] = []

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x `mod` 10 + x `div` 10 + sumDigits(xs)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (sumDigits $ doubleEveryOther $ toRevDigits n) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a) 



