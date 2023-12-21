-- ================================
-- parametric polymorphism
-- ================================

-- NOTE: review
-- All Haskell functions must be parametric in their type parameters;
-- the functions must not care or make decisions based on the choices for these parameters.
-- A function can’t do one thing when a is `Int` and a different thing when a is `Bool`.
-- Haskell simply provides no facility for writing such an operation.
-- This property of a language is called parametricity.

-- REF: https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/
-- REF: https://wiki.haskell.org/Polymorphism
-- REF: https://wiki.haskell.org/Generics

-- There are many deep and profound consequences of parametricity.
-- One consequence is something called type erasure.
-- Because a running Haskell program can never make decisions based on type information,
-- all the type information can be dropped during compilation.
-- Despite how important types are when writing Haskell code,
-- they are completely irrelevant when running Haskell code.

-- Another consequence of parametricity is that it restricts what polymorphic functions you can write.
-- Look at this type signature:
strange :: a -> b
-- The `strange` function takes a value of some type `a` and produces a value of another type `b`.
-- But, crucially, it isn’t allowed to care what `a` and `b` are!
-- Thus, there is no way to write strange!
strange = error "impossible!"

-- What about this one?
limited :: a -> a
-- This function must produce an `a` when given an `a`.
-- There is only one `a` it can produce – the one it got!
-- Thus, there is only one possible definition for limited:
limited x = x

-- In general, given the type of a function,
-- it is possible to figure out various properties of the function just by thinking about parametricity.
-- The function must have some way of producing the output type…
-- but where could values of that type come from?
-- By answering this question, you can learn a lot about a function.

-- ================================
-- total and partial functions
-- ================================

-- Functions which will crash for certain inputs are called partial functions.
-- Functions which have certain inputs that will make them recurse infinitely are also called partial.
-- Functions which are well-defined on all possible inputs are known as total functions.

-- REF: https://wiki.haskell.org/Partial_functions
-- REF: https://wiki.haskell.org/Avoiding_partial_functions
-- REF: https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/

-- ================================
-- recursion patterns
-- ================================

-- map: Perform some operation on every element of the list.
addOneToAll :: [Int] -> [Int]
addOneToAll [] = []
addOneToAll (x : xs) = x + 1 : addOneToAll xs

absAll :: [Int] -> [Int]
absAll [] = []
absAll (x : xs) = abs x : absAll xs

squareAll :: [Int] -> [Int]
squareAll [] = []
squareAll (x : xs) = x ^ 2 : squareAll xs

-- The thing that changes is the operation we want to perform on each element of the list.
-- We can specify this operation as a function of type `a -> a`.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

addOneToAll' xs = map (+ 1) xs

exampleListMap = addOneToAll' []

exampleListMap' = addOneToAll' [-1, 2, 6]

-- filter: Keep only some elements of the list, and throw others away, based on a test.
keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x : xs)
  | x > 0 = x : keepOnlyPositive xs
  | otherwise = keepOnlyPositive xs

keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x : xs)
  | even x = x : keepOnlyEven xs
  | otherwise = keepOnlyEven xs

-- The thing to abstract out is the test (or predicate) used to determine which values to keep.
-- A predicate is a function of type `a -> Bool`
-- which returns `True` for those elements which should be kept,
-- and `False` for those which should be discarded.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

keepOnlyEven' xs = filter even xs

exampleListFilter = keepOnlyEven' []

exampleListFilter' = keepOnlyEven' [-1, 2, 6]

-- fold: “Summarize” the elements of the list somehow (find their sum, product, maximum…).
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x : xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- As usual, the idea will be to abstract out the parts that vary,
-- aided by the ability to define higher-order functions.
fold :: (a -> b -> b) -> b -> [a] -> b
fold f initValue [] = initValue
fold f initValue (x : xs) = f x (fold f initValue xs)

-- Notice how `fold` essentially replaces `[]` with `initValue`, and `(:)` with `f`, that is:
-- fold f initValue [a, b, c] == a `f` (b `f` (c `f` initValue))
sum'' xs = fold (+) 0 xs

product'' xs = fold (*) 1 xs

length'' xs = fold addOne 0 xs
  where
    addOne _ initValue = 1 + initValue

-- `fold` is already provided in the standard Prelude:
-- foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
-- foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

-- In general, however, you should use `foldl'` from `Data.List` instead,
-- which does the same thing as `foldl` but is more efficient.

-- ================================
-- functional programming
-- ================================

-- functional combinators: The `(.)` operator, part of the Haskell Prelude, is just function composition.
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g x = f (g x)

-- Say we want to take every element of a list and add 1 and then multiply by 4.
-- Here is a good way to do it:
add1Mul4 :: [Int] -> [Int]
add1Mul4 x = map ((* 4) . (+ 1)) x

-- While we’re at it, we should also show the `($)` operator, which has a trivial-looking definition:
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- Why have such a thing? Because `($)` is parsed as an operator,
-- and this is useful for avoiding parentheses.
-- For example, if we wish to negate the number of even numbers in a list, we could say:
negateNumEvens1 :: [Int] -> Int
negateNumEvens1 x = negate (length (filter even x))

-- or
negateNumEvens2 :: [Int] -> Int
negateNumEvens2 x = negate $ length $ filter even x

-- lambda: It is sometimes necessary to create an anonymous function, or lambda expression.

-- Say we want to duplicate every string in a list:
duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where
    dup x = x ++ x

-- It’s a tiny bit silly to name `dup`. Instead, we can make an anonymous function:
duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

-- The backslash binds the variables after it in the expression that follows the `->`.
-- For anything but the shortest examples, it’s better to use a named helper function, though.

-- ================================
-- currying
-- ================================

-- Remember how the types of multi-argument functions look weird,
-- like they have “extra” arrows in them? For example, consider the function:
f :: Int -> Int -> Int
f x y = 2 * x + y

-- I promise that there is a beautiful, deep reason for this, and now it’s finally time to reveal it:
-- all functions in Haskell take only one argument.
-- Say what?! But doesn’t the function f shown above take two arguments?
-- No, actually, it doesn’t: it takes one argument (an Int) and outputs a function (of type `Int -> Int`);
-- that function takes one argument and returns the final answer.
-- In fact, we can equivalently write f’s type like this:
f' :: Int -> (Int -> Int)
f' x y = 2 * x + y

-- In particular, note that function arrows associate to the right,
-- that is, `W -> X -> Y -> Z` is equivalent to `W -> (X -> (Y -> Z))`.
-- We can always add or remove parentheses around the rightmost top-level arrow in a type.

-- Function application, in turn, is left-associative.
-- That is, `f 3 2` is really shorthand for `(f 3) 2`.
-- This makes sense given what we said previously about `f` actually taking one argument and returning a function:
-- we apply `f` to an argument `3`,
-- which returns a function of type `Int -> Int`,
-- namely, a function which takes an `Int` and adds 6 to it.
-- We then apply that function to the argument `2` by writing `(f 3) 2`,
-- which gives us an `Int`.
-- Since function application associates to the left, however,
-- we can abbreviate `(f 3) 2` as `f 3 2`,
-- giving us a nice notation for `f` as a “multi-argument” function.

-- The “multi-argument” lambda abstraction:
-- \x y z -> ...
-- is really just syntax sugar for:
-- \x -> (\y -> (\z -> ...))
-- Likewise, the function definition:
-- f x y z = ...
-- is syntax sugar for:
-- f = \x -> (\y -> (\z -> ...))

-- This idea of representing multi-argument functions as one-argument functions returning functions is known as currying,
-- named for the British mathematician and logician Haskell Curry.
-- (His first name might sound familiar; yes, it’s the same guy.)
-- Curry lived from 1900-1982 and spent much of his life at Penn State—but he also helped work on ENIAC at UPenn.
-- The idea of representing multi-argument functions as one-argument functions returning functions
-- was actually first discovered by Moses Schönfinkel,
-- so we probably ought to call it schönfinkeling.
-- Curry himself attributed the idea to Schönfinkel,
-- but others had already started calling it “currying” and it was too late.

-- REF: https://en.wikipedia.org/wiki/Currying

-- If we want to actually represent a function of two arguments
-- we can use a single argument which is a tuple.
-- That is, the function:
-- f'' :: (Int, Int) -> Int
-- f'' (x, y) = 2 * x + y
-- can also be thought of as taking “two arguments”,
-- although in another sense it really only takes one argument which happens to be a pair.

-- In order to convert between the two representations of a two-argument function,
-- the standard library defines functions called `curry` and `uncurry`,
-- defined like this (except with different names):
schönfinkel :: ((a, b) -> c) -> a -> b -> c
schönfinkel f x y = f (x, y)

unschönfinkel :: (a -> b -> c) -> (a, b) -> c
unschönfinkel f (x, y) = f x y

-- `uncurry` in particular can be useful when you have a pair and want to apply a function to it.
-- For example:
-- Prelude> uncurry (+) (2, 3)
-- 5

-- ================================
-- partial application
-- ================================

-- NOTE: review
-- The fact that functions in Haskell are curried makes partial application particularly easy.
-- The idea of partial application is that we can take a function of multiple arguments
-- and apply it to just some of its arguments,
-- and get out a function of the remaining arguments.
-- But as we’ve just seen, in Haskell there are no functions of multiple arguments!
-- Every function can be “partially applied” to its first (and only) argument,
-- resulting in a function of the remaining arguments.

-- Note that Haskell doesn’t make it easy to partially apply to an argument other than the first.
-- The one exception is infix operators,
-- which as we’ve seen, can be partially applied to either of their two arguments using an operator section.
-- In practice this is not that big of a restriction.
-- There is an art to deciding the order of arguments to a function to make partial applications of it as useful as possible:
-- the arguments should be ordered from “least to greatest variation”,
-- that is, arguments which will often be the same should be listed first,
-- and arguments which will often be different should come last.

-- ================================
-- wholemeal application
-- ================================

-- Let’s put some of the things we’ve just learned together
-- in an example that also shows the power of a “wholemeal” style of programming.
-- Consider the function `foobar`, defined as follows:
foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x : xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

-- This seems straightforward enough, but it is not good Haskell style.
-- The problem is that it is
-- - doing too much at once; and
-- - working at too low of a level.

-- Instead of thinking about what we want to do with each element,
-- we can instead think about making incremental transformations to the entire input,
-- using the existing recursion patterns that we know of.
-- Here’s a much more idiomatic implementation of `foobar`:
foobar' :: [Integer] -> Integer
foobar' = sum . map ((+ 2) . (* 7)) . filter (> 3)

-- This defines `foobar'` as a “pipeline” of three functions:
-- first, we throw away all elements from the list which are not greater than three;
-- next, we apply an arithmetic operation to every element of the remaining list;
-- finally, we sum the results.

-- Notice that in the above example, `map` and `filter` have been partially applied.
-- For example, the type of `filter` is `(a -> Bool) -> [a] -> [a]`.
-- Applying it to `(>3)` (which has type `Integer -> Bool`) results in a function of type `[Integer] -> [Integer]`,
-- which is exactly the right sort of thing to compose with another function on `[Integer]`.

-- ================================
-- point-free style
-- ================================

-- The style of coding in which we define a function without reference to its arguments
-- (in some sense saying what a function is rather than what it does)
-- is known as “point-free” style.

-- But taken too far it can become extremely confusing.
-- `lambdabot` in the `#haskell` IRC channel has a command `@pl`
-- for turning functions into equivalent point-free expressions;
-- here’s an example:
-- @pl \f g x y -> f (x ++ g x) (g y)
-- join . ((flip . ((.) .)) .) . (. ap (++)) . (.)
-- This is clearly not an improvement!

-- Consider the following two functions:
mumble = (`foldr` []) . ((:) .)

grumble = zipWith ($) . repeat

-- Can you figure out what these functions do?
-- What if I told you that they are both equivalent to the `map` function.
-- These are great examples of how point-free style can be taken too far.
-- For this reason, some people refer to it as point-less style.
