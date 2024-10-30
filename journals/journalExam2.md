# Haskell
## Purity
A *pure function* in maths is a function where each f(x) always produces the same y value. Here there are no observable effects besides computing y, and no external state dependency. For example:
```
pure :: Int -> Int
pure x = x + 1

pure 1 -- this will ALWAYS produce 2
```

This relates to the term *computational side effects* which is when an action occurs in a computation that isn't part of the computation's expected or stated action. Think of it as any operation that does more than just compute a value. This matters because it makes behavior unpredictable. 

*Referential transparency* is a property where for a given parameter value, a function always returns the same result. As a consqeuence, expressions can be directly replaced with their values. This enables equational reasoning about code. For example:
```
-- Referentially transparent
let x = 5 + 5 in x * 2
-- Can be replaced with
let x = 10 in x * 2
-- Can be replaced with
20

-- Not referentially transparent
let x = getLine in x ++ x  -- Results may differ
```

## Laziness
*Lazy evaluation* essentially works by only evaluating expressions only when they are needed, instead of evaluating everything at once. Take the below square function.
```
sqr :: Num a => a -> a
sqr x = x * x
```
With eager evaluation, the call `sqr (sqr (3 + 4))` would be evaluated as such:
```
# Eager (inside out)
sqr (sqr (3 + 4))
    = sqr (sqr 7)
    = sqr (7 * 7)
    = sqr 49
    = 49 * 49
    = 2401
```
Buttttt with lazy evaluation, it is instead evaluated as:
```
# Lazy (outside in)
sqr (sqr (3 + 4))
    = let x = sqr (3 + 4) 
        in x * x
    = let y = 3 + 4
        in let x = y * y
            in x * x
    = let x = 49
        in x * x
    = 2401 
```
This has many performance considerations.
- Advantages
    - Since each expression is only evaluated when needed, this allows for better efficiency by avoiding unneeded computations.
    - This also enables modularity, as the generation of data is seperated from its consumption.
    - We can work with infinite lists, since we only generate the needed elements instead of trying to approximate infinity
- Disadvantages
    - Delayed computations can accumulate and consume more memory. Additionally, it is harder to predict when and where computations occur.
    - This increates the potential space complexity, making it harder to predict memory usage and can result in space leaks.
    - Time complexity is also impacted, as performance is less predictable due to hidden evaluation costs.

## Input/Output
Haskell uses the *IO* type to encapsulate side-effects, helping ensure that pure functions remain unaffected. It does this by isolating these side-effects in a *first-order type*, having a *\*->\** kind. It is a type constructor that takes in a type and returns an IO action. For example, `IO -> Int`.

The IO type implements multiple type classes.
1. Functors
    - Maps over computation results using `fmap`
2. Applicative
    - Enables sequencing of `IO` actions using `<*>` and `pure`.
3. Monad
    - Provides a way to chain `IO` actions using bind `>>=` and `do` notation.

Here are some practical examples that show these off.
```
-- Functors and fmap
main :: IO ()
main = do
    putStrLn "Enter some text:"
    upperStr <- fmap (map toUpper) getLine
    putStrLn ("Uppercase: " ++ upperStr)

-- Applicative and <*> and pure
main :: IO ()
main = do
    putStrLn "Enter your first name:"
    firstName <- getLine
    putStrLn "Enter your last name:"
    lastName <- getLine
    let fullName = (++) <$> pure firstName <*> pure (" " ++ lastName)
    putStrLn ("Your full name is: " ++ fullName)
-- pure (++) wraps the (++) in the IO context, pure firstName and pure (" " ++ lastName) wraps the strings, and <*> applies the wrapped function inside IO to the wrapped values.

-- Monad and do
main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

-- Monad and bind (>>=)
main :: IO ()
main =
    putStrLn "Enter a number:" >>
    getLine >>= \input ->
    let number = read input :: Int
        result = number * 2
    in putStrLn ("The double is: " ++ show result)
```

## Data Definitions
We can declare type synonyms using `type`. Essentially it's like using names to access data types. For example
```
type City = String
type TestCase = [City]
```
We can also declare synonyms for type constructors. For example, `type Pair a = (a, a)`

If we are using our card game as an example, they would be declared as the following.
```
data Suit = Hearts | Diamonds | Clubs | Spades
data Card = Card Int Suit -- Card 5 Hearts
```
We can also use this to declare types to craft domain-specific languages. Like using synonyms to declare querys and statements for SQL.

# Functors
## Basics
A *functor* applies a function over a wrapped value without altering the structure itself. In Haskell, these are types

There is a term involed that refers to *'lifting'* a function. This means applying a regular function to a value inside a context (list, `Maybe`) using `fmap`. Think of Functors acting as a bridge that lets us apply regular functions to values inside a 'box' without needing to open it. Through *lifting* a function to it can operate on the value inside using `fmap` or `<$>`, this allows pure functions to work with values inside a context such as `Maybe`, `List`, or `IO` without breaking the functional programming paradigm of Haskell.

## Class Functor
```
class Functor f where
    fmap :: (a -> b) -> (f a -> f b)
```
Only higher-order types, types of kind *\* -> \** (takes one type parameter), can be an instance of `Functor`. Being an *algebraic data type*, this means that it is formed by combining other types (`Maybe`, `List`, `Either`).

The `fmap` function has a type signature `fmap :: Functor f => (a -> b) -> f a -> f b`, which maps a function over a functor.
```
map length :: [[a]] -> [Int]
fmap length ::  f a -> f b
            ::  Maybe [a] -> Maybe Int
```

A convience operator for `fmap` is the `<$>` operator, which is a infix synonym for `fmap`.
```
(<$>)  :: Functor f => (a -> b) -> f a -> f b     -- fmap
(<$)   :: Functor f => a -> f b -> f a            -- replace all values
```

## Functor Laws
We have two laws for functor. We have the *identity law* and the *composition law*.
- Identity law
    - `fmap id = id`
    - This means that mapping the identity function over a functor leaves it unchanged
- Composition Law
    - `fmap (f . g) = fmap f . fmap g`
    - This means that mapping a composition of functions is the same as composing the mappings
    - Note that we won't have to prove this in the exam

We can apply these to common functor types like `List` and `Maybe`
```
fmap id [1,2,3] == [1,2,3]
fmap (f . g) [x] == (fmap f . fmap g) [x]

fmap id (Just 5) == Just 5
fmap (f . g) (Just x) == (fmap f . fmap g) (Just x)

```

## List Instance
For a list, `fmap` is similar to `map`. It has a type signature, `map :: (a -> b) -> [a] -> [b]`. For example, `fmap (*2) [1,2,3]` results in `[2,4,6]`

Lets recall how we implement `map`.
```
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
```

## Maybe instance
With `Maybe`, functor is implemented as the following.
```
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

fmap (+1) (Just 5) -- Results in Just 6
fmap (+1) Nothing  -- Results in Nothing
```

## Card example
```
type Rank = Int
data Suit = Hearts | Diamonds | Clubs | Spades
data Card a = Card a Suit
instance Functor Card where
    fmap f (Card n s) = Card (f n) s
```

# Applicative Functors
## Basics
*Applicative functors* are an extension of functors, allowing functions that take multiple arguments to be applied to a Functor-wrapped value. This allows for functions that are themselves in a context to be applied to values in a context. Where functors apply normal functions to wrapped values, applicative ones apply wrapped functions to wrapped values. The term *apply* refers to the ability to apply wrapped functions to wrapped values.
```
-- Using fmap and then the apply operator
fmap (*2) [1,2,3] = [2,4,6]
(*2) <$> [1,2,3] = [2,4,6] -- Using the inflix operator for fmap

[(*2), (+10)] <*> [1,2,3] = [2,4,6,11,12,13]
```

## Class Applicative
```
class Functor f => Applicative f where
    pure  :: a -> f a                       -- Embeds a value into the applicative functor
    u <*> v :: f (a -> b) -> (f a -> f b)   -- Applies a functor-wrapped function to a functor-wrapped value
```
Like functors, only higher-order types (kind *\* -> \**) can be applicative functors.

Here are some convenience operators for applicative
```
(*>)   :: Applicative f => f a -> f b -> f b      -- sequence, keep right
(<*)   :: Applicative f => f a -> f b -> f a      -- sequence, keep left
(<**>) :: Applicative f => f a -> f (a -> b) -> f b  -- reversed (<*>)
```
## Laws for Applicative Functors
For these, we just need to be able to recognize which is which in the exam and match them.

1. Identity Law
    - `pure id <*> v = v`
    - Applying the wrapped identity function should equal original value
    - This ensures that `pure` doesn't modify structure
2. Homomorphism Law
    - `pure f <*> pure x = pure (f x)`
    - Applying pure function to a pure value equals a pure result
    - This shows that `pure` preserves function application
3. Interchange Law
    - `u <*> pure y = pure ($ y) <*> u`
    - This shows that the order of evaulation doesn't matter
    - Provides flexibility in application order
4. Composition Law
    - `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
    - This ensures that composed applications work correctly
    - It maintains function composition properties

Example of identity with `List`
```
pure id <*> u = u  -- Identity
[id] <*> [1,2,3] = [1,2,3]
```
Example of composition with `List`
```
-- Normally left-associative
(([(.)] <*> [even]) <*> [(*2), (+10)]) <*> [1,2,3]
= [even] <*> ([(*2), (+10)] <*> [1,2,3]) -- Applying even to the result of the right parentheses.
```
Example of identity with `Maybe`
```
pure id <*> Just x == Just x
```

## List Instance
The `pure` definition for lists. This always creates a single-element list.
```
pure :: a -> [a]
pure x = [x]
```
The `(<*>)` operator for lists. This applies each function to each value, resulting in all possible combinations
```
(<*>) :: [a -> b] -> [a] -> [b]
fs <*> xs = [f x | f <- fs, x <- xs]
```
Some practical applications.
```
pure (+1) <*> [1,2,3]      -- [2,3,4]
[(+1),(*2)] <*> [1,2,3]    -- [2,3,4,2,4,6]
```

## Maybe Instance
The `pure` definition for Maybe. Wraps value in a Just constructor. Nothing remains as Nothing.
```
pure :: a -> Maybe a
pure x = Just x
```
The `(<*>)` operator for Maybe. Propagates Nothing, and applies function if both are Just.
```
(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
Nothing <*> _ = Nothing
_ <*> Nothing = Nothing
(Just f) <*> mx = fmap f mx
```
Some practical applications.
```
Just (+3) <*> Just 2        -- Just 5
Just (+3) <*> Nothing       -- Nothing
Nothing <*> Just 2          -- Nothing
```

# Equational Reasoning
## Substitution
Substitution just involves swapping out part of an expression that matches a known definition or prior result. For example,
```
Map
1. map f [] = [] -- Empty case
2. map f (x:xs) = f x : map f xs -- Non empty case
Func composition
1. (f . g) x = f (g x)

-- Matches know definition, so we substitute it in to reduce our expression
map (f . g) []
since (f.g) can be 'f'
= {map.1}
map 'f' []
= {map.1}
[]
```

## Basic Proof Construction
Proofs involve a good amount of what's on the wiki under 'useful definitions'. About a 1/3rd.
We have two columns that show the working for each side of the equations.
For our proof that `sum (xs ++ ys) = sum xs + sum ys`, we can construct it as follows.

`sum ([] ++ ys) = sum [] + sum ys` (base case where xs is an empty list)
```
Definitions:
    sum [] = 0
    sum (x:xs) = x + sum xs
```
| sum ([] ++ ys)    | sum [] + sum ys |
| ----------------- | --------------- |
| sum ([] ++ ys)    | sum [] + sum ys |
| ={ (++).1 }       | ={ sum.1 }      |
| sum (ys)          | 0 + sum ys      |
| sum ys            | sum ys          |


## Induction and inferences
To solve the proof, we need to identify what cases to construct. For example, do we need a base case? Then the next inductive case, if needed? For example:
- When induction is needed
    - Recursive data types such as lists
    - Recursive functions
    - Properties over infinite structure (infinite lists, lazy evaluation properties)
- When it isn't needed
    - Finite cases (fixed-size structures, boolean properties, finite lists)
    - Direct equalities (simple value comparisons, constructor pattern matches, non-recursive properties, etc)
Within this inductive case (list of xs or x:xs, etc.), we also need to see where we can use our *inductive hypothesis*, or when we can substitute in what we're trying to prove.

For example, the inductive case for our sum proof. `sum (xs ++ ys) = sum xs + sum ys`
```
P(x:xs)
sum ((x:xs) ++ ys) = sum (x:xs) + sum ys
Definitions:
    sum [] = 0
    sum (x:xs) = x + sum xs

    IH is where we can substitute in the original proof. I.e. sum (xs ++ ys) = sum xs + sum ys
```
| sum ((x:xs) ++ ys)                    |  sum (x:xs) + sum ys            |
| --------------------------------------|---------------------------------|
| ={ (++).2 }                           |  ={ sum.2 }                     |
| sum (x : (xs ++ ys))                  |  x + sum xs + sum ys            |
| ={ sum.2 }                            |  ""                             |
| x + sum (xs ++ ys)                    |  ""                             |
| = { IH }                              |  ""                             |
| x + sum xs + sum ys                   |  x + sum xs + sum ys            |

