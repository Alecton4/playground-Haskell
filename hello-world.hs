-- ================================
-- function
-- ================================
doubleMe x = x + x

-- Functions in Haskell don't have to be in any particular order,
-- so it doesn't matter if you define `doubleMe` first and then `doubleUs` or if you do it the other way around.
-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

{-
The difference between Haskell's if statement and if statements in imperative languages is that
the else part is mandatory in Haskell.
In imperative languages you can just skip a couple of steps if the condition isn't satisfied
but in Haskell every expression and function must return something.
-}

-- Another thing about the if statement in Haskell is that it is an expression.
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

-- We usually use `'` to either denote a strict version of a function (one that isn't lazy)
-- or a slightly modified version of a function or a variable.
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- When a function doesn't take any parameters,
-- we usually say it's a definition (or a name).
-- Because we can't change what names (and functions) mean once we've defined them,
-- conanO'Brien and the string "It's a-me, Conan O'Brien!" can be used interchangeably.
conanO'Brien = "It's a-me, Conan O'Brien!"

-- ================================
-- list comprehension
-- ================================
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10]]

rightTriangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

rightTriangles'24 = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]

-- ================================
-- type system
-- ================================
factorial :: Integer -> Integer
factorial n = product [1 .. n]

demoNum = fromIntegral (length [1, 2, 3, 4]) + 3.2

-- demoNum = length [1,2,3,4] + 3.2

-- What is this `a`? Is it a type?
-- Remember that we previously stated that types are written in capital case,
-- so it can't exactly be a type.
-- Because it's not in capital case it's actually a type variable.
itself :: a -> a
itself x = x

-- A typeclass is a sort of interface that defines some behavior.
-- If a type is a part of a typeclass,
-- that means that it supports and implements the behavior the typeclass describes.
-- Everything before the `=>` symbol is called a class constraint.
-- We can read the type declaration like this:
-- the `testEq` function takes any two values that are of the same type and returns a Bool.
-- The type of those two values must be a member of the Eq class (this was the class constraint).
testEq :: (Eq a) => a -> a -> Bool
testEq x y = x == y

-- ================================
-- pattern matching
-- ================================
luckyFour :: (Integral a) => a -> String
luckyFour 4 = "LUCKY NUMBER FOUR!"
luckyFour x = "Sorry, you're out of luck, pal!"

-- We can also define a factorial function recursively.
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- Pattern matching can also be used on tuples.
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
-- Notice that if you want to bind to several variables
-- (even if one of them is just `_` and doesn't actually bind at all),
-- we have to surround them in parentheses.
-- From Phind:
-- > `head' x : _ = x` will be problematic
-- > because Haskell will interpret it as `(head' x) : _ = x`.
head' (x : _) = x

inspectList :: (Show a) => [a] -> String
inspectList [] = "The list is empty"
inspectList (x : []) = "The list has one element: " ++ show x
inspectList [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
inspectList (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- We already implemented our own `length` function using list comprehension.
-- Now we'll do it by using pattern matching and a little recursion.
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_ : xs) = 1 + length'' xs

-- There's also a thing called as patterns.
-- Those are a handy way of breaking something up according to a pattern
-- and binding it to names whilst still keeping a reference to the whole thing.
-- You do that by putting a name and an `@` in front of a pattern.
-- For instance, the pattern `xs@(x:y:ys)`.
-- This pattern will match exactly the same thing as `x:y:ys`
-- but you can easily get the whole list via `xs`
-- instead of repeating yourself by typing out `x:y:ys` in the function body again.
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- One more thing — you can't use `++` in pattern matches.
-- If you tried to pattern match against `(xs ++ ys)`,
-- what would be in the first and what would be in the second list? It doesn't make much sense.
-- It would make sense to match stuff against `(xs ++ [x,y,z])` or just `(xs ++ [x])`,
-- but because of the nature of lists, you can't do that.
-- From Phind:
-- > Pattern matching in Haskell works by matching the structure of data,
-- > not by executing code or expressions.
-- > The `++` operator is a function that concatenates two lists,
-- > and it doesn't represent a structure that can be matched.
-- firstOne ([x] ++ xs) = x
-- lastOne (xs ++ [x]) = x

-- ================================
-- guards
-- ================================
-- Whereas patterns are a way of making sure a value conforms to some form and deconstructing it,
-- guards are a way of testing whether some property of a value (or several of them) are true or false.
-- Guards are indicated by pipes that follow a function's name and its parameters.
-- Note that there's no `=` right after the function name and its parameters, before the first guard.
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- ================================
-- `where` bindings
-- ================================
-- Note that `where` bindings aren't shared across function bodies of different patterns.
-- If you want several patterns of one function to access some shared name, you have to define it globally.
bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

-- We could have done this pattern matching directly in the function's parameters
-- (it would have been shorter and clearer actually)
-- but this just goes to show that it's possible to do it in `where` bindings as well.
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

-- Just like we've defined constants in where blocks, you can also define functions.
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

{-
`where` bindings can also be nested.
It's a common idiom to make a function and define some helper function in its `where` clause
and then to give those functions helper functions as well,
each with its own `where` clause.
-}

-- ================================
-- `let` bindings
-- ================================
-- `let` bindings let you bind to variables anywhere
-- and are expressions themselves, but are very local,
-- so they don't span across guards.
-- The difference is that `let` bindings are expressions themselves.
-- `where` bindings are just syntactic constructs.
demoLet = 4 * (let a = 9 in a + 1) + 2

-- If we want to bind to several variables inline,
-- we obviously can't align them at columns.
-- That's why we can separate them with semicolons.
demoLet' = (let a = 100; b = 200; c = 300 in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)

-- You can also put `let` bindings inside list comprehensions.
-- We include a `let` inside a list comprehension much like we would a predicate,
-- only it doesn't filter the list, it only binds to names.
-- The names defined in a `let` inside a list comprehension are visible to the output function (the part before the |)
-- and all predicates and sections that come after of the binding.
-- We can't use the `bmi` name in the `(w, h) <- xs` part because it's defined prior to the `let` binding.
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- calcBmis'' xs = [bmi | (w, h) <- xs, bmi >= 25.0, let bmi = w / h ^ 2]

{-
We omitted the `in` part of the `let` binding when we used them in list comprehensions
because the visibility of the names is already predefined there.
However, we could use a `let` in binding in a predicate
and the names defined would only be visible to that predicate.
-}

{-
Since `let` bindings are expressions and are fairly local in their scope,
they can't be used across guards.
Some people prefer `where` bindings because the names come after the function they're being used in.
That way, the function body is closer to its name and type declaration and to some that's more readable.
-}

-- ================================
-- case expressions
-- ================================
-- Pattern matching on parameters in function definitions is actually just syntactic sugar for case expressions.
-- The following two pieces of code do the same thing and are interchangeable.
headAgain [] = error "No head for empty lists!"
headAgain (x : _) = x

headAgain' xs = case xs of
  [] -> error "No head for empty lists!"
  (x : _) -> x

-- Whereas pattern matching on function parameters can only be done when defining functions,
-- case expressions can be used pretty much anywhere.
describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

-- Because pattern matching in function definitions is syntactic sugar for case expressions,
-- we could have also defined this like the following.
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where
    what [] = "empty."
    what [x] = "a singleton list."
    what xs = "a longer list."
