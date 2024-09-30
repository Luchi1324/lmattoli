# Types and Classes
## Haskell concrete types
Some examples of concrete types
- Int: limited precision integers in the range [-2^29, 2^29). Overflow isn't detected.
- Integer: arbitrary-precision integers, these are unbounded so larger than Int
- Rational: arbitrary-precision rational numbers
- Float: single-precision floating-point numbers (6-7 digits precision)
- Double: double-precision floating-point numbers (15-16 digits precision)
- Char: a single unicode character
- Bool: simple boolean value
## Higher-order types
A **higher order type** (type constructor) are types that take other types as a parameter to produce a new type. For example, a `[Int]` list is a list that takes in multiple `Int`'s to create an list of Ints. It's like higher order functions that take in functions.

Some common examples
- List ([])
    - Literal examples
    - `[1,2,3,4]     :: [Int]`
    - `['a','b','c'] :: [Char]`
- Tuple (Groups a fixed number of elements)
    - Literal examples
    - `(1, 'a')              :: (Int, Char)`
    - `("Hello", True, 3.14) :: (String, Bool, Double)`

**Kinds** are types of types. These describe the type signature of type constructors
Some basic kinds
- `*` (Star): Represents a concrete type that has values
    - `Int`, `Bool`, `[Int]` all have kind `*`
- `* -> *`: Type constructor that takes one argument to produce a concrete type
    - `[]` (list), `IO` all have kind `* -> *`
- `* -> * -> *`: Type constructor that takes two type arguments
    - `Either`, `(,)` (Tuple), `Map` all have kind `* -> * -> *`

GHCi `:kind` examples
```
:kind Int           -- Int :: *
:kind Maybe         -- Maybe :: * -> *
:kind Either        -- Either :: * -> * -> *
:kind []            -- [] :: * -> *
:kind (,)           -- (,) :: * -> * -> *
:kind IO            -- IO :: * -> *
:kind (Int, Maybe String) -- (Int, Maybe String) :: *

```
## Common type classes
**Type classes** in Haskell provide to define a generic interface that can be implemented by different types. We can specify a set of functions that must be defined for a type to be considered an instance of that class. This enables ploymorphism (functions that operate on any type that implements a particular interface)

Definition Example:
```
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

Some examples of common type classes w/ essential functions, example instances, and usage.
- `Eq`: Defines equality and inequality operations
```
(==) :: Eq a => a -> a -> Bool  -- Essential functions
(/=) :: Eq a => a -> a -> Bool
------------------------------
instance Eq Person where
  (Person name1 age1) == (Person name2 age2) = name1 == name2 && age1 == age2
------------------------------
5 == 5        -- True
"Hello" /= "Hi" -- True

```

- `Ord`: Defines a total ordering for types
```
compare :: Ord a => a -> a -> Ordering
(<) :: Ord a => a -> a -> Bool  -- Essential functions
(<=) :: Ord a => a -> a -> Bool
(>) :: Ord a => a -> a -> Bool
(>=) :: Ord a => a -> a -> Bool
-------------------------------------
instance Ord Person where
  compare (Person _ age1) (Person _ age2) = compare age1 age2
-------------------------------------
3 < 5         -- True
"apple" > "banana" -- False

```

- `Enum`: Represents types that have predecessors and successors, typically used for sequentially ordered types
```
succ :: Enum a => a -> a                     -- Essential functions
pred :: Enum a => a -> a
toEnum :: Enum a => Int -> a
fromEnum :: Enum a => a -> Int
enumFrom :: Enum a => a -> [a]
enumFromThen :: Enum a => a -> a -> [a]
enumFromTo :: Enum a => a -> a -> [a]
enumFromThenTo :: Enum a => a -> a -> a -> [a]
-----------------------------------------------
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Show, Enum)

-- Haskell automatically derives Enum for Day
-----------------------------------------------
succ Monday       -- Tuesday
[Monday .. Friday] -- [Monday, Tuesday, Wednesday, Thursday, Friday]
```

- `Show`: Allows a type to be converted to a human-readable `String`
```
show :: Show a => a -> String  -- Essential functions
-----------------------------
instance Show Person where
  show (Person name age) = name ++ " is " ++ show age ++ " years old."
-----------------------------
show 123          -- "123"
show True         -- "True"
show [1, 2, 3]    -- "[1,2,3]"
```

- `Read`: Allows a type to be parsed from a `String`
```
read :: Read a => String -> a  -- Essential functions
------------------------------
read "123" :: Int     -- 123
read "True" :: Bool   -- True
read "[1,2,3]" :: [Int] -- [1,2,3]
```

- `Num`: Represents numeric types that support basic arithmetic operations
```
(+) :: Num a => a -> a -> a  -- Essential functions
(-) :: Num a => a -> a -> a
(*) :: Num a => a -> a -> a
negate :: Num a => a -> a
abs :: Num a => a -> a
signum :: Num a => a -> a
fromInteger :: Num a => Integer -> a
-------------------------------------
data MyNumber = MyNumber Int

instance Num MyNumber where
  (MyNumber a) + (MyNumber b) = MyNumber (a + b)
  (MyNumber a) - (MyNumber b) = MyNumber (a - b)
  (MyNumber a) * (MyNumber b) = MyNumber (a * b)
  negate (MyNumber a) = MyNumber (negate a)
  abs (MyNumber a) = MyNumber (abs a)
  signum (MyNumber a) = MyNumber (signum a)
  fromInteger n = MyNumber (fromInteger n)
-------------------------------------
5 + 3       -- 8
10 * 2      -- 20
negate (-4) -- 4
```

- `Fractional`: Represents fractional (non-integer) numbers supporting division. Note that `Fractional` is a subclass of `Num`
```
(/) :: Fractional a => a -> a -> a            -- Essential functions
recip :: Fractional a => a -> a
fromRational :: Fractional a => Rational -> a
----------------------------------------------
instance Fractional MyFraction where
  (MyFraction a) / (MyFraction b) = MyFraction (a / b)
  recip (MyFraction a) = MyFraction (recip a)
  fromRational r = MyFraction (fromRational r)
----------------------------------------------
3.0 / 2.0     -- 1.5
recip 4.0     -- 0.25
fromRational (3 % 4) -- 0.75
```

- `Integral`: Represents integral (whole) types supporting division that disgards the remainder. Note that `Integral` is a subclass of `Real` and `Num`
```
quot :: Integral a => a -> a -> a          -- Essential functions
rem :: Integral a => a -> a -> a
div :: Integral a => a -> a -> a
mod :: Integral a => a -> a -> a
quotRem :: Integral a => a -> a -> (a, a)
divMod :: Integral a => a -> a -> (a, a)
toInteger :: Integral a => a -> Integer
-----------------------------------------
instance Integral MyInt where
  quot (MyInt a) (MyInt b) = MyInt (a `quot` b)
  rem (MyInt a) (MyInt b) = MyInt (a `rem` b)
  toInteger (MyInt a) = toInteger a
-----------------------------------------
10 `quot` 3   -- 3
10 `rem` 3    -- 1
div 10 3      -- 3
mod 10 3      -- 1
toInteger 5   -- 5
```

- `Floating`: Represents floating-point types supporting transcendental functions. Note that `Floating` is a subclass of `Fractional`
```
pi :: Floating a => a        -- Essential functions
exp :: Floating a => a -> a
log :: Floating a => a -> a
sin :: Floating a => a -> a
cos :: Floating a => a -> a
asin :: Floating a => a -> a
acos :: Floating a => a -> a
atan :: Floating a => a -> a
sinh :: Floating a => a -> a
cosh :: Floating a => a -> a
asinh :: Floating a => a -> a
acosh :: Floating a => a -> a
atanh :: Floating a => a -> a
------------------------------
instance Floating MyFloat where
  pi = ... -- Define as needed (i.e. pi to x decimals)
  exp x = ... -- Implement exponential
  log x = ... -- Implement logarithm
  -- ... and so forth
------------------------------
sin pi      -- Approximately 0
exp 1       -- Approximately 2.718281828459045
log (exp 1) -- 1
```

We can create instances of type classes by implementing all the essential functions defined by said class.
1. First we declate the data type
```
data Color = Red | Green | Blue
```
2. Then we declare the instance, w/ essential functions
```
instance Eq Color where
  (==) :: Color -> Color -> Bool
  Red == Red     = True
  Green == Green = True
  Blue == Blue   = True
  _ == _         = False
  -- Inequality operator is infered, but we can optinally define it
  (/=) :: Color -> Color -> Bool
  x /= y = not (x == y)
```

Type classes also allow us to constrain the types functions can accept. `funcName :: (TypeClass1 a, TypeClass2 a) => a -> a -> ReturnType
```
-- Simple display value function
display :: Show a => a -> String
display x = "Value: " ++ show x

-- Parse value
parseValue :: Read a => String -> a
parseValue s = read s

-- We can also use multiple constraints to support multiple type classes
complexFunction :: (Num a, Ord a, Show a) => a -> String
complexFunction x =
  let y = x * 2
  in if y > 10 then show y else "Too small"

-- A generic function to check if all elements in a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

```
### Type Synonyms
We can use type synonyms to give alternate names for an existing type. For example, if we want to make a tuple for hours and times.
- `type Hours = Int`
- `type Minutes = Int`
- `type Time = (Hours, Minutes)`

And a function to return the time.
```
showTime :: Hours -> Minutes -> Time
showTime hours minutes = (hours, minutes)
```
We can use type synonyms to help craft domain-specific languages (DSLs). For example, if we wanted to represent SQL...
```
type TableName = String
type ColumnName = String
type Value = String

data Condition = Equals ColumnName Value | And Condition Condition | Or Condition Condition
type Query = TableName -> [Condition] -> String

-- Rest of logic would go here
```
The benefits of this is that it helps ensure the code is readable, simplifies complex types, aids in the design of DSLs, and it allows us to focus on more higher-level concepts by abstracting away implementation details.
### Recognize type errors
Usually these occur for a given set of reasons
- Type mismatch. For example, calling a function that returns an `Int` on a function that expects a `String`
- Incorrect number of arguments. For example, calling `isEven :: Int -> Bool` with `isEven 4 2`
- Type class constraint violations. For example, 

# Lists
## Properties of Haskell lists
At it's core, all Haskell lists are implemented as a singly linked list. 
- It provides efficient sequential access (head, tail operations). This does mean that random access is inefficient, O(n) time complexity when compared to data structures like arrays, or vectors from C++. 
- Additionally, all lists are homogeneous which means that they must all contain the same type. This ensures type safety and consistency.
- Given that lists are essentially singly linked lists, this also means that the length of a Haskell list is not stored as part of its structure. So determining the length of the list also requires O(n) time complexity. As an example...
```
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs
```
## Recursive nature of list ADT
Haskell lists are fundamentally recursive data structures, which enables elegant and concise operations via recursion.
- In a typical recursion function...
    - We can use the empty list `[]` as a base case ...
    - ... and the cons operator `(:)` as a recursive case.
    - For example, the list notation `[1,2,3]` is an abbreviation for a more basic form `1:2:3:[]`. Think of it as building up the list via recursion.
```
sumList :: [Int] -> Int
sumList [] = 0  -- Base case
sumList (x:xs) = x + sumList xs
-------------------------------
headElement :: [a] -> a
headElement (x:_) = x  -- Recursive case
headElement []    = error "Empty list"

```
## Common functions on lists
Haskell already has a rich set of Prelude functions that can manipulate lists. Here are some of the most important ones.
- `length`: returns the number of elements in a list. `length :: [a] -> Int`
    - `length [1,2,3,4]    -- Returns 4`
- `null`: checks of a list. `null :: [a] -> Bool`
    - `null [1,2,3,4]   -- Returns False`
- `head`: retrieves the first element of a list. `head :: [a] -> a`
    - `head [1,2,3]     -- Returns 1`
- `tail`: retrieves all elements of a list except the first. `tail [a] -> [a]`
    - `tail [1,2,3]     -- Returns [2,3]`
- `last`: retrieves the last element of a list. `last :: [a] -> a`
    - `last [1,2,3]     -- Returns 3`
- `init`: retrieves all elements of a list except the last. `init :: [a] -> [a]`
    - `init [1,2,3]     -- Returns [1,2]`
- `append (++)` and `concat`: Concatenates two list. `(++) :: [a] -> [a] -> [a]`
    - `[1,2] ++ [3,4]  -- Returns [1,2,3,4]`
- `take`: Retrieves first `n` elements from a list. `take :: Int -> [a] -> [a]`
    - `take 3 [1,2,3,4,5]   -- Returns [1,2,3]`
- `drop`: Removes first `n` elements from a list. `drop :: Int -> [a] -> [a]`
    - `drop 2 [1,2,3,4,5]   -- Returns [3,4,5]`
- `reverse`: Reverses the order of elements in a list. `reverse :: [a] -> [a]`
    - `reverse [1,2,3,4,5]  -- Returns [5,4,3,2,1]`
- `sort`: Sorts elements of a list in ascending order. `sort :: Ord a => [a] -> [a]`
    - `sort [3,5,2,4,1]     -- Returns [1,2,3,4,5]`
- `nub`: Removes duplicate elements from a list. `nub :: Eq a => [a] -> a`
    - `nub [1,2,2,3,4,4]    -- Returns [1,2,3,4]`
## Know list patterns
Lists can be worked on via pattern matching. This allows us to break down a list into parts, which allows for recursive operations
- As mentioned before, the pattern `[]` is an empty list.
- The pattern `(x:xs)` is a non empty list, and binds `x` to the head and `xs` to the tail.
- We also have a singleton pattern `[x]` which matches a list that contains exactly one element.
## Understand how to recur-on-list
We can use these patterns to operate on different elements of a list. For example, here's a basic function to sum up a list with pattern matching.
```
sumList :: [Int] -> Int
sumList [] = 0                      -- Base case, if there are no elements then just 0
sumList (x:xs) = x + sumList xs     -- Recursive case, add the head to the list to the sums of the rest of the list
```

A more advanced example using guards which filters elements based on a predicate.
```
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []                  -- _ is a wildcard, essentially its 'discard whatever predicate we have, we just want the list'
myFilter p (x:xs)                   -- p is our predicate, and ()
  | p x       = x : myFilter p xs   -- If p x is True, we include x and recurse on xs
  | otherwise = myFilter p xs       -- Otherwise, we exclude x and recurse on xs
```
## List sugar
Haskell offers a nice set of syntactic conviences to make list operations more readable and expressive. (which is nice Haskell already took me a long long time to get used to)
- Using **ranges** to generate a list with sequential elements
    - `[1..5]` makes `[1,2,3,4,5]`
    - `['a'..'d']` makes `['a','b','c','d']`
    - `[2,4..10]` makes `[2,4,6,8,10]`
    - `[10,8..2]` makes `[10,8,6,4,2]`
    - `[1..]` makes `[1,2,3,4...] to infinity!`
- We also have list **comprehensions**, which can generate a list based on existing list which is similar to set comprehentions in mathematics (discrete maths PTSD ;-;)
- Structure: `[expression | qualifiers]`
- Square of even numbers from 1 - 10: `sqOfEvens = [x^2 | x <- [1..10], even x]`
    - Makes `[4, 16, 36, 64, 100]`
- Cartesian product of two lists: `cartesianProduct = [(x,y) | x <- [1,2], y <- ['a','b']]`
    - Makes `[(1,'a'), (1,'b'), (2,'a'), (2,'b')]`
- Triplets, with z generated from x + y: `[(x, y, z) | x <- [1..3], y <- [1..3], let z = x + y, z <= 4]`
    - Makes `[(1,1,2), (1,2,3), (1,3,4), (2,1,3), (2,2,4), (3,1,4)]`

# Functional Tropes
## Thinking recursively
**Recursion** is the fundamental concept of any functional programming language. If functional programming was a church, any type of loops would be the devil. The key idea is that functions will call upon themselves to solve smaller instances of a problem.
In any recursive function regardless of its purpose, it will always consist of two things.
1. The Base Case
    - This is the simpliest instance of a problem, which is solved directly with no further recursion.
    - This also prevents us from infinitely solving a problem, and acts as a stop.
    - For example, with a Factorial function we want to stop when we reach 0 where the result is just 1. Otherwise it becomes a function to discover -∞, and it will find it at the heat death of the universe.
```
factorial :: Int -> Int
factorial 0 = 1                       -- Base case
factorial n = n * factorial (n - 1)   -- Recursive case

```
2. The Recursive Case
    - This is where our recursive function breaks down the problem into smaller, managable sub-problems and calls itself to solve it.
    - For example, if we want to sum up the numbers in a list we call the function to find the sum of the rest of the list if there is still a tail.
    - It works like `1 + sumList [2,3] ==> 1 + 2 + sumList [3] ==> 1 + 2 + 3 + sumList [] ==> 1 + 2 + 3 + 0 ==> 6`
```
sumList :: [Int] -> Int
sumList [] = 0                    -- Base case
sumList (x:xs) = x + sumList xs   -- Recursive case
```
## Mapping: `map :: (a -> b) -> [a] -> [b]`
**Mapping** involves applying a function to each elements of a list, and produces a new list with these new elements.
- This is useful because mapping replaces loops in imperative languages to perform repetitive tasks
- For example, lets say we wanted to double all of the elements in a list, covert strings to uppercase, or get the lengths of each sublist in a list.
```
import Data.Char (toUpper)
doubleList :: [Int] -> [Int]
doubleList xs = map (*2) xs
--------------------------------
upperList :: [String] -> [String]
upperList strs = map (map toUpper) strs
--------------------------------
lengths :: [[a]] -> [Int]
lengths lists = map length lists
--------------------------------
-- doubleList [1, 2, 3]  -- Returns [2, 4, 6]
-- uppercaseList ["hello", "world"]  -- Returns ["HELLO", "WORLD"]
-- lengths [[1,2], [3], [4,5,6]]  -- Returns [2, 1, 3]
```
## Filtering: `filter :: (a -> Bool) -> [a] -> [a]`
**Filtering** involves selecting elements from a list that satisfy a given predicate, and gives us a new list of only these elements
- This can perform search operations by defining our predicate as our search criteria
- For example, finding all even number in a list, or elements with more than 3 characters
```
evenNumber :: [Int] -> [Int]
evenNumber xs = filter even xs
------------------------------
longStrings :: [String] -> [String]
longStrings strs = filter (\s -> length s > 3) strs
------------------------------
-- evenNumbers [1, 2, 3, 4, 5]  -- Returns [2, 4]
-- longStrings ["hi", "hello", "hey", "world"]  -- Returns ["hello", "world"]
```
## Folding: foldr `foldr :: (a -> b -> b) -> b -> [a] -> b` & foldl `foldl :: (b -> a -> b) -> b -> [a] -> b`
**Folding** reduces a list to a single cumulative value by iteratively applying a function. Imagine that you are literally 'folding' the list to make one result.
- Folding is used to perform aggregate computations such as sum, product, maximum, and more.
- There are two different approaches to fold a list. Either a left fold, or a right fold.
    - Left fold (foldl) takes in an **accumlator** of type `b` and an **element** of type `a`, and returns a new accumulated value of type `b`
    - Right fold (foldr) takes in an **element** of type `a` and an **accumlator** of type `b`, and returns a new accumlated value of type `b`
    - The main difference from the two is the approach they take. Left fold starts from the beginning of the list(left-associative), and the right fold starts from the end of the list(right associative).
    - Because of this, foldl cannot work with infinite lists as it requires traversing the list in its entirety. However, foldr can if the function allows for short-circuiting.
    - With Haskell's lazy evaluation, foldr can handle infinite list gracefully. If we check that any element can satisfy a condition in a list, when it encounters an element that does it stops evaulating. Foldl, on the other hand, will never terminate.
    - Foldr is well-suited for elements that build new lists since it can prepend elements efficiently. Foldl is well-suited for aggregate computations since it processes elements from the beginning of the list to its end. While stack overflow is a concern, there is a function `foldl'` which is a stricter verision that mitigates this. 
```
      Left Fold                              Right Fold
0 + 1 : [2,3,4,5]                  | 1:[2,3,4,5]
(0 + 1) + 2:[3,4,5]                | 1 + 2:[3,4,5]
((0 + 1) + 2) + 3:[4,5]            | 1 + (2 + 3:[4,5])
(((0 + 1) + 2) + 3) + 4:[5]        | 1 + (2 +(3 + 4:[5]))
((((0 + 1) + 2) + 3) + 4) + 5:[]   | 1 + (2 +(3 +(4 + 5:[])))
                                   | 1 + (2 +(3 +(4 +(5 + 0))))
          15                       |          15
```
- Here's an example of how foldr1 and foldl1 differ in operation. (Note: foldr1 and foldl1 just use the first element of the list as the initial accumlator value, `foldr1 :: (a -> a -> a) -> [a] -> a`)
```
foldr1 max [3, 1, 4, 2]               | foldl1 max [3, 1, 4, 2]
= max 3 (foldr1 max [1, 4, 2])        | = foldl1 max [3, 1, 4, 2]
= max 3 (max 1 (foldr1 max [4, 2]))   | = max (max (max 3 1) 4) 2
= max 3 (max 1 (max 4 2))             | = max (max 3 4) 2
= max 3 (max 1 4)                     | = max 4 2
= max 3 4                             | = 4
= 4                                   |
```
- For example, getting the product of a list, finding the maximum element, or concatenating a list of strings
```
productList :: [Int] -> Int
productList xs = foldr (*) 1 xs
-----------------------------------
maximumList :: (Ord a) => [a] -> a
maximumList xs = foldr1 max xs
-----------------------------------
concatStrings :: [String] -> String
concatStrings strs = foldr (++) "" strs
-----------------------------------
-- productList [1, 2, 3, 4]  -- Returns 24
-- maximumList [3, 1, 4, 2]  -- Returns 4
-- concatStrings ["Hello, ", "World", "!"]  -- Returns "Hello, World!"
```
## Zipping: zip `zip :: [a] -> [b] -> [(a,b)]` & zipWith `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`
**Zipping** combines elements from two lists into a single lists of tuples, pairing corresponding elements.
- This is useful when we want to combine two lists into one, either as tuples or applying a function to corresponding elements.
- `zip` takes in two lists, `zipWith` takes in a function with two elements of type `a` and `b`, and returns type `c`, then takes in two lists.
- For example, we can merge two lists into a single list of pairs, or create a list of boolean flags if an element is greater than the other.
```
compareLists :: [Int] -> [Int] -> [Bool]
compareLists xs ys = zipWith (>) xs ys
-----------------------------------------
mergeLists :: [String] -> [Int] -> [(String, Int)]
mergeLists names ages = zip names ages
-----------------------------------------
-- compareLists [5, 3, 8] [2, 4, 7]  -- Returns [True, False, True]
-- mergeLists ["Alice", "Bob"] [25, 30]  -- Returns [("Alice",25), ("Bob",30)]
```
