{-# OPTIONS_GHC -Wall #-}

module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
  deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
  deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
-- exactMatches xs ys = sum [if x == y then 1 else 0 | x <- xs, y <- ys]
-- exactMatches xs ys = sum [i | x <- xs, y <- ys, let i = if x == y then 1 else 0]
-- NOTE: The above two implementations are wrong.
exactMatches xs ys = sum (zipWith (\x y -> if x == y then 1 else 0) xs ys)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
-- From Phind:
-- > `map` applies the lambda function to each `Peg` in `colors`.
-- > The lambda function uses `filter` to create a list of elements in `code`
-- > that are equal to `color`,
-- > and `length` to count the number of elements in this list.
-- > The result is a list of counts of each `Peg` in `code`,
-- > in the same order as `colors`.
countColors code = map (\color -> length (filter (== color) code)) colors

-- `countColors` can be separated into two functions:
-- countColors :: Code -> [Int]
-- countColors code = map (countSingleColor code) colors
-- countSingleColor :: Code -> Peg -> Int
-- countSingleColor code color = length (filter (== color) code)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum (zipWith min (countColors xs) (countColors ys))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
  where
    exact = exactMatches secret guess
    nonExact = matches secret guess - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) code =
  (exact == exactMatches guess code) && (nonExact + exact == matches guess code)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter $ isConsistent move

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
