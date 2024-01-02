-- ================================
-- declarations and variables
-- ================================

-- `=` does not denote “assignment” like it does in many other languages.
-- Instead, `=` denotes definition, like it does in mathematics.
-- That is, `x = 4` should not be read as “x gets 4” or “assign 4 to x”,
-- but as “x is defined to be 4”.

-- Q: What does the following mean?
y = y + 1

-- ================================
-- basic types
-- ================================

-- `Int`s are guaranteed by the Haskell language standard to accommodate values at least up to \(\pm 2^{29}\),
-- but the exact size depends on your architecture.
-- For example, on my 64-bit machine the range is \(\pm 2^{63}\).

-- Find the range on your machine by evaluating the following.
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

-- The `Integer` type, on the other hand, is limited only by the amount of memory on your machine.

-- arbitrary-precision integers
reallyBig :: Integer
reallyBig = 2 ^ (2 ^ (2 ^ (2 ^ 2)))

numDigits :: Int
numDigits = length (show reallyBig)

-- For floating-point numbers, there is `Double`.

-- double-precision floating point
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

-- There is also a single-precision floating point number type, `Float`.

-- Finally, there are booleans, characters, and strings.

-- booleans
b1, b2 :: Bool
b1 = True
b2 = False

-- unicode characters
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- Strings are lists of characters with special syntax.
s :: String
s = "Hello, Haskell!"

-- ================================
-- arithmetic
-- ================================

ex01 = 3 + 2

ex02 = 19 - 27

ex03 = 2.35 * 8.6

ex04 = 8.7 / 3.1

ex05 = mod 19 3

ex06 = 19 `mod` 3

ex07 = 7 ^ 222

ex08 = (-3) * (-7)

-- Addition is only between values of the same numeric type,
-- and Haskell does not do implicit conversion. You must explicitly convert with:
-- - `fromIntegral`: converts from any integral type (`Int` or `Integer`) to any other numeric type.
-- - `round`, `floor`, `ceiling`: convert floating-point numbers to `Int` or `Integer`.
int1 :: Int
int1 = 1

int2 :: Integer
int2 = 2

-- badArith1 = int1 + int2

-- `/` performs floating-point division only.
-- For integer division we can use `div`.
-- badArith2 = int1 / int1

ex09 = int1 `div` int1

ex10 = 12 `div` 5

-- ================================
-- Boolean logic
-- ================================

ex11 = True && False

ex12 = not (False || True)

ex13 = ('a' == 'a')

ex14 = (16 /= 3)

ex15 = (5 > 3) && ('p' <= 'q')

ex16 = "Haskell" > "C++"

exIf = if 5 > 3 then "5 > 3" else "5 <= 3"

-- Idiomatic Haskell does not use if expressions very much,
-- often using pattern-matching or guards instead.

-- ================================
-- defining basic functions
-- ================================

-- We can write functions on integers by cases.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

-- Choices can also be made based on arbitrary Boolean expressions using guards.
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

-- ================================
-- pairs
-- ================================

p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

-- Haskell also has triples, quadruples, … but you should never use them.
-- As we’ll see next week, there are much better ways to package three or more pieces of information together.

-- ================================
-- functions with multiple arguments
-- ================================

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

ex17 = f 3 17 8

-- The above example applies the function `f` to the three arguments `3`, `17`, and `8`.
-- Note also the syntax for the type of a function with multiple arguments,
-- like `Arg1Type -> Arg2Type -> ... -> ResultType`.
-- This might seem strange to you (and it should!). Why all the arrows?
-- Wouldn’t it make more sense for the type of `f` to be something like `Int Int Int -> Int`?
-- Actually, the syntax is no accident: it is the way it is for a very deep and beautiful reason,
-- which we’ll learn about in a few weeks; for now you just have to take my word for it!

-- Note that function application has higher precedence than any infix operators!

-- ================================
-- lists
-- ================================

nums, range, range2 :: [Integer]
nums = [1, 2, 3, 19]
range = [1 .. 100]
range2 = [2, 4 .. 100]

-- Haskell (like Python) also has list comprehensions.

-- Strings are just lists of characters.
-- That is, `String` is just an abbreviation for `[Char]`,
-- and string literal syntax (text surrounded by double quotes) is just an abbreviation for a list of Char literals.

-- `hello1` and `hello2` are exactly the same.
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

-- The simplest possible list is the empty list.
emptyList = []

-- Other lists are built up from the empty list using the cons operator, `(:)`.
-- Cons takes an element and a list, and produces a new list with the element prepended to the front.
ex18 = 1 : []

ex19 = 3 : (1 : [])

ex20 = 2 : 3 : 4 : []

ex21 = [2, 3, 4] == 2 : 3 : 4 : []

-- Note also that these are really singly linked lists, NOT arrays.

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- We can write functions on lists using pattern matching.
intListLength :: [Integer] -> Integer
intListLength [] = 0
-- intListLength (_ : xs) = 1 + intListLength xs
intListLength (x : xs) = 1 + intListLength xs

-- We can also use nested patterns.
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = [] -- Do nothing to the empty list
sumEveryTwo (x : []) = [x] -- Do nothing to lists with a single element
-- sumEveryTwo (x : y : zs) = (x + y) : sumEveryTwo zs
sumEveryTwo (x : (y : zs)) = (x + y) : sumEveryTwo zs

-- ================================
-- conbining functions
-- ================================

-- It’s good Haskell style to build up more complex functions by combining many simple ones.

-- The number of hailstone steps needed to reach 1 from a starting number.
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

-- This may seem inefficient to you:
-- it generates the entire hailstone sequence first and then finds its length,
-- which wastes lots of memory… doesn’t it? Actually, it doesn’t!
-- Because of Haskell’s lazy evaluation, each element of the sequence is only generated as needed,
-- so the sequence generation and list length calculation are interleaved.
-- The whole computation uses only O(1) memory, no matter how long the sequence.
-- (Actually, this is a tiny white lie, but explaining why (and how to fix it) will have to wait a few weeks.)

-- We’ll learn more about Haskell’s lazy evaluation strategy in a few weeks.
-- For now, the take-home message is: don’t be afraid to write small functions that transform whole data structures,
-- and combine them to produce more complex functions.
-- It may feel unnatural at first, but it’s the way to write idiomatic (and efficient) Haskell,
-- and is actually a rather pleasant way to write programs once you get used to it.

-- ================================
-- `let` expressions
-- ================================

-- To define a local variable scoped over an expression, use `let`.
strLength :: String -> Int
strLength [] = 0
strLength (_ : xs) =
  let len_rest = strLength xs
   in len_rest + 1

-- ================================
-- `where` clauses
-- ================================

-- To define a local variable scoped over multiple guarded branches, use `where`.
frob :: String -> Char
frob [] = 'a' -- len is NOT in scope here
frob str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where
    len = strLength str

-- In idiomatic Haskell code, `where` is somewhat more common than `let`,
-- because using `where` allows the programmer to get right to the point in defining what a function does,
-- instead of setting up lots of local variables first.

-- REF: https://wiki.haskell.org/Let_vs._Where

-- ================================
-- Haskell layout
-- ================================

-- Haskell is a whitespace-sensitive language.
-- Haskell uses indentation level to tell where certain regions of code end,
-- and where new statements appear.
-- The basic idea is that, when a so-called layout herald appears,
-- GHC looks at the next thing it sees and remembers its indentation level.
-- A later line that begins at the exact same indentation level is considered another member of the group,
-- and a later line that begins at a lesser (more to the left) indentation level is not part of the group.

-- The layout heralds are `where`, `let`, `do`, `of`, and `case`.
-- Because Haskell modules begin with `module Name where`,
-- that means that the layout rule is in effect over the declarations in the file.

-- When calculating indentation level,
-- tabs in code are considered with tab stops 8 characters apart, regardless of what your editor might show you.
-- This potential confusion is why tabs are a terrible, terrible idea in Haskell code.

-- ================================
-- accumulators
-- ================================

-- NOTE: review
-- Haskell’s one way to repeat a computation is recursion.
-- Recursion is a natural way to express the solutions to many problems.
-- However, sometimes a problem’s structure doesn’t exactly match Haskell’s structure.
-- For example, say we have a list of numbers, that is, an `[Int]`.
-- We wish to sum the elements in the list, but only until the sum is greater than 20.
-- After that, the rest of the numbers should be ignored.
-- Because recursion over a list builds up the result from the end backward,
-- a naive recursion will not work for us.
-- What we need is to keep track of the running sum as we go deeper into the list.
-- This running sum is called an accumulator.
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums -- the acc. starts at 0
  where
    go :: Int -> [Int] -> Int
    go acc [] = acc -- empty list: return the accumulated sum
    go acc (x : xs)
      | acc >= 20 = acc
      | otherwise = go (acc + x) xs
