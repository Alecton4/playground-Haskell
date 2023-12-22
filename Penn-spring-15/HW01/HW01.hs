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
hanoi 1 startPeg endPeg _ =
  [(startPeg, endPeg)]
hanoi n startPeg endPeg tmpPeg =
  hanoi (n - 1) startPeg tmpPeg endPeg
    ++ [(startPeg, endPeg)]
    ++ hanoi (n - 1) tmpPeg endPeg startPeg

-- Exercise 7 -----------------------------------------

-- BUG: This one seems wrong.
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 startPeg endPeg _ _ =
  [(startPeg, endPeg)]
hanoi4 2 startPeg endPeg tmpPeg1 _ =
  [(startPeg, tmpPeg1), (startPeg, endPeg), (tmpPeg1, endPeg)]
hanoi4 n startPeg endPeg tmpPeg1 tmpPeg2 =
  hanoi4 (n - 2) startPeg tmpPeg1 endPeg tmpPeg2
    ++ [(startPeg, tmpPeg2)]
    ++ [(startPeg, endPeg)]
    ++ hanoi4 (n - 2) tmpPeg1 endPeg startPeg tmpPeg2

-- NOTE: This one is not optimal.
hanoi4' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4' 1 startPeg endPeg _ _ =
  [(startPeg, endPeg)]
hanoi4' 2 startPeg endPeg tmpPeg1 _ =
  [(startPeg, tmpPeg1), (startPeg, endPeg), (tmpPeg1, endPeg)]
hanoi4' n startPeg endPeg tmpPeg1 tmpPeg2 =
  hanoi4' (n - 2) startPeg tmpPeg1 endPeg tmpPeg2
    ++ [(startPeg, tmpPeg2)]
    ++ [(startPeg, endPeg)]
    ++ [(tmpPeg2, endPeg)]
    ++ hanoi4' (n - 2) tmpPeg1 endPeg startPeg tmpPeg2

-- TODO: Implement the optimal one.
