{-# OPTIONS_GHC -Wall #-}

module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit num = num `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit num = num `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits num
  | num <= 0 = []
  | otherwise = lastDigit num : toRevDigits (dropLastDigit num)

toDigits :: Integer -> [Integer]
toDigits num = reverse (toRevDigits num)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [x, y * 2]
doubleEveryOther (x : y : zs) = x : y * 2 : doubleEveryOther zs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigitOneInt

sumDigitOneInt :: Integer -> Integer
sumDigitOneInt num
  | num < 10 = num
  | otherwise = num `mod` 10 + sumDigitOneInt (num `div` 10)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn num = (sumDigits $ doubleEveryOther $ reverse $ toDigits num) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 startPeg endPeg tmpPeg = [(startPeg, endPeg)]
hanoi n startPeg endPeg tmpPeg =
  hanoi (n - 1) startPeg tmpPeg endPeg
    ++ [(startPeg, endPeg)]
    ++ hanoi (n - 1) tmpPeg endPeg startPeg
