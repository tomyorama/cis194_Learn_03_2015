{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10


-- Drop the last digit from a number

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x 
    |x>0 = (x `mod` 10):toRevDigits(x `div` 10)
    |otherwise = []
toDigits = reverse.toRevDigits
-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs
doubleEveryOther x = x

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits x= sum (map (sum.toRevDigits) x)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x =(((sumDigits.doubleEveryOther. toRevDigits) x) `mod` 10)==0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from help to = (hanoi (n-1) from to help) ++ [(from,to)] ++ (hanoi (n-1) help from to
