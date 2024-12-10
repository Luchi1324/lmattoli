# Monoids
## Moniod Definitions
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

{-
  -- Memorize these for exam
  x <> mempty = x               -- Right identity laws
  mempty <> x = x               -- Left identity laws
  (x <> y) <> z = x <> (y <> z) -- Associativity law
-}
```

## Example monoid proof
```haskell
{-
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
-}
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