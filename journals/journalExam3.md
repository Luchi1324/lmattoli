# Monoids
A **monoid** is a fundamental algebraic structure that captures the concept of combining things together in a systematic and consistent way. To say that a type forms a monoid means that we have a way of taking two values of that type and merging them into a single value, and that we also have a special *neutral* element which leaves an element unchanged when merged with it.

Formally, a monoid consists of:
1. An **identity element**. Usually `mempty`, all this element does is when you combine it with a value, nothing happens. Like with addition, the identity element is `0` because adding it to any number leaves it unchanged.
2. An **associative binary operation**. Usually `(<>)` or `mappend`, this takes in two values of the same type and produces a new value of that type. Associativeity means that the order of applying this operation doesnt matter. `(x <> y) <> z` is the same as `x <> (y <> z)`.

Monoids are useful as they can abstract the act of "combindation", so instead of needing to specify logic to accumulate values, we can just create a monoid instance to fold a list of values into a single result in a predictible way. This has applications such as adding numbers, concatenating strings, merging logging messages, and such.

## Class Definitions
```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)
  mconcat [] = mempty
  mconcat (x:xs) = mappend x (mconcat xs)
  x <> y = mappend x y
```

## Monoid Laws
```haskell
  -- Memorize these for exam
  x <> mempty = x               -- Right identity laws
  mempty <> x = x               -- Left identity laws
  (x <> y) <> z = x <> (y <> z) -- Associativity law
```

## Monoid Examples
### Lists
```haskell
mempty = []
(<>)/mappend = (++) -- List concatenation

[1,2,3] <> [4,5] = [1,2,3,4,5]
[] <> [4,5] = [4,5]                 -- shows identity
([1,2] <> [3]) <> [4] = [1,2,3,4]
[1,2] <> ([3] <> [4]) = [1,2,3,4]   -- shows associativity
```

### Sum
```haskell
mempty = Sum 0
(<>)/mappend (Sum x) (Sum y) = Sum (x + y)

Sum 10 <> Sum 20 <> Sum 5 = Sum (10 + 20 + 5) = Sum 35
Sum 0 <> Sum 5 = Sum 5                                  -- identity
```

### Product
```haskell
mempty = Product 1
(<>)/mappend = (Product x) (Product y) = Product (x * y)

Product 2 <> Product 3 <> Product 4 = Product (2 * 3 * 4) = Product 24
Product 1 <> Product 5 = Product 5                       -- identity
```

### Maybe Monoid
If the type inside `Maybe` is a monoid, the `Maybe` itself is also a monoid.
```haskell
mempty = Nothing
(<>)/mappend = --applies the monoid operation directly to x and y

Just (Sum 10) <> Just (Sum 5) = Just (Sum 15)
Nothing <> Just (Sum 3) = Just (Sum 3)        -- identity
Just (Sum 7) <> Nothing = Just (Sum 7)        -- identity
```


### Example proof
```haskell
  mempty <> mempty <> mappend (Sum 3) (Sum 4)
  (mempty <> mempty) <> mappend (Sum 3) (Sum 4)
  ={ right identity }
  mempty <> mappend (Sum 3) (Sum 4)
  ={ left identity }
  mappend (Sum 3) (Sum 4)
  ={ mappend.1 }
  Sum (3 + 4)        -- Good but WHNF (weak head normal form)
  ={ maths }
  Sum 7              -- NF
```

# Monads
A **monad** provides a framework for chaining computations that have some sort of context or effect. This allows us to connect multiple operations that produce or consume values "wrapped" in a context (think possible failure `Maybe`, non-determinism `[]`, `IO` operations, or error handling `Either`). In purely functional languages such as Haskell, we don't perform side effects directly, but rather we model effects (like I/O, handling failure, or maintaining state) as values with certain 'effectful' contexts.

Monads build upon previous abstractions called functors and applicatives. To rehash, a **functor** lets us apply a pure function to values wrapped in a context (like `Maybe` or `[]`), and an **applicative** extends that ability by allowing functions themselves to be context-wrapped and applied. A **monad** gives us a way to chain operations together so that each step can depend on results produced by previous steps, all while remaining in the same context or effect.

## Class Definition
```haskell
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

## Monad Laws
```haskell
return a >>= f = f a                        -- Left Identity (inject a value a into the monad and bind it to f, its the same as applying f to a directly)
m >>= return = m                            -- Right Identity (If you bind a monadic value m to return, you get m back unchanged)
(m >>= f) >>= g = m >>= (\x -> f x >>= g)   -- Associativity (Changing the grouping of your changed operations doesnt change the overall result)
```

While a Monad are also Monoids, this doesn't necessarly imply that every Haskell `Monad` instance must also be a `Monoid`.

## Do Notation
`do` notation allows us to write monadic code in a imperative style instead of strictly functional.
```haskell
main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let n = read input :: Int
  putStrLn ("You entered: " ++ show n)
```

This is syntatic sugar for the following:
```haskell
main :: IO ()
main =
  putStrLn "Enter a number:" >>= \_ ->
  getLine >>= \input ->
  let n = read input :: Int in
  putStrLn ("You entered: " ++ show n)
```

## Monad Examples
### Maybe Monad
Computations that may fail
```haskell
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

Just 10 >>= (\x -> safeDivide x 2) -- Just 5.0
Just 10 >>= (\x -> safeDivide x 0) -- Nothing
```

### Either Monad
Computations that may fail w/ an error message
```haskell
divideEither :: Double -> Double -> Either String Double
divideEither _ 0 = Left "Division by zero!"
divideEither x y = Right (x / y)

Right 10 >>= (\x -> divideEither x 2) -- Right 5.0
Right 10 >>= (\x -> divideEither x 0) -- Left "Division by zero!"
```

### List Monad
Non-deterministic computations with multiple results
```haskell
[1,2,3] >>= \x -> [x, x*2]
-- For each element x in [1,2,3], produce [x, x*2]
-- = [1,2,2,4,3,6]
```

### I/O Monad
Allows sequencing of input/output actions while keeping purity in the language
```haskell
main = do
  name <- getLine
  putStrLn ("Hello, " ++ name)

-- A functional approach w/o do notation
main = 
  getLine >>= \name ->
  putStrLn ("Hello, " ++ name)
```

# Zippers
**Zipper** are a more concrete data structure concept used to navigate and update complex data structures efficiently. Lets say that we have a data structure like a list or a tree and you want to "focus" on one particular element, making modifications or extracting values from that point. Normally, moving to a certain position in a list or making changes in a tree might require a lot of reconstruction. A zipper provides a clever representation that stores not just the current point of focus, but also the *context* needed to return back to or move around the entire structure.

A zipper works by splitting a data structure into two parts:
1. The currently focused element.
2. Everything else, represented in such a way that we can efficiently recombine it or shift our focus.

## List Zipper
A list zipper might represent the currently focused element and the elements to the left and right.
- For example: `( [elems to the left], focused element, [elems to the right] )`

In more detail:
```haskell
-- Suppose we have a list [1,2,3,4] and we want to focus on the element '2'.
-- A possible list zipper representation might look like:
([1], 2, [3,4])

-- Moving focus to the right:
-- ([1], 2, [3,4]) -> ([1,2], 3, [4])

-- Changing the current element:
-- ([1,2], 3, [4]) replaced with ([1,2], 5, [4]) changes the focus from 3 to 5.

```

This is much more efficient than normal list operations. Where shifting focus or updating the middle element usually costs `O(n)`, moving the focus left or gith and updating the focused element with zippers only costs `O(1)`.

## Tree Zipper
A tree zipper works by storing the current node and a path back up the tree, which includes sibling and parent nodes. 
- With tree zippers, you can move up, down, left, or right in a tree and modify the focused node without reconstructing the entire tree from scratch.

In the context of a binary tree, a tree zipper could look like this:
```haskell
-- Focus on a node, remember its parent and siblings in a structure:
data Tree a = Leaf a | Node a (Tree a) (Tree a)

-- A tree zipper might include the focused node and a list of “crumbs” that
-- record what was above it:
type Breadcrumb a = (a, Tree a, Tree a)   -- parent value, left subtree, right subtree
type Zipper a = (Tree a, [Breadcrumb a])

-- With this you can go up, down, etc.
```


## Example zipWith proof
```haskell
{-
 
  (zipWith (^) [1..] [2,4..]) !! 2
    -- Definitions
    zipWith f [] ys = []
    zipWith f xs [] = []
    zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys
 
    (x:_) !! 0 = x
    (_:xs) !! n = xs !! (n - 1)
 
 |   -- Solve
 |   (zipWith (^) [1..] [2,4..]) !! 2
 |   ={ zipWith.3 }
 |   ((1^2) : zipWith (^) [2..] [4,6..]) !! 2
 |   ={ (!!).2 }
 |   (zipWith (^) [2..] [4,6..]) !! (2-1)
 |   ={ maths } (demo purposes, just do the arithmetic if there are no other steps involved)
 |       ""       ""     ""     !!   1
 |   ={ zipWith.3 }
 |   ((2^4) : zipWith (^) [3..] [6,8..]) !! 1
 |   ={ (!!).2 }
 |   (zipWith (^) [3..] [6,8..]) !! (1-1)
 |   ={ maths }
 |       ""         ""     ""    !!   0
 |   ={ zipWith.3 }
 |   ((3^6) : zipWith (^) [4..] [8,10..]) !! 0
 |   ={ (!!).1 }
 |   3^6
 V   ={ maths }
 NF  729
-}
```

# Monads

## Random IO examples
```haskell
print :: Show a => a -> IO ()
main = do
  putStrLn "Enter a word: "
  s <- getLine
  print (length s)

-- (>>) :: m a -> m b -> m b
-- (>>=)_ :: m a -> (a -> m b) -> m b

main = putStrLn "Enter.." >> (getLine >>= (print.length))
--     String -> IO            IO[Char]   ([a] -> IO ())
```