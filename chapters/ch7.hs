sqr :: Num a => a -> a
sqr x = x * x

{- Types of evaulation for sqr
# Eager (inside out)
sqr (sqr (3 + 4))
    = sqr (sqr 7)
    = sqr (7 * 7)
    = sqr 49
    = 49 * 49
    = 2401

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
-}

f :: a -> b -> a
f x y = x

-- If we think of the leftmost '=' as computer calculations, notice how there are less computations since we only want x
{- Evaulation of f
# Eager
f 5 (sqr (sqr (3 + 4)))
    = f 5 (sqr (sqr 7))
    = f 5 (sqr 49)
    = f 5 2401
    = 5


# Lazy
f 5 (sqr (sqr (3 + 4)))
    = let x = 5
        y = sqr (sqr (3 + 4))
        in x
    = 5
Since y is not needed in the original function (f x y = x), we just get x directly and ignore y
-}

example :: Integer
example = head (map (*10) ([1,2,3] ++ [4..]))

{- Evaluation of example
head (map (*10) ([1,2,3] ++ [4..]))
    -- head
    = let (x:_) = map (*10) ([1,2,3] ++ [4..])
        in x
    -- map
    = let f = (*10) 
        (y:ys) = ([1,2,3] ++ [4..])
        in let (x:_) = f y : map f ys
            in x
    = let (z:zs) = 1 : ([2,3] ++ [4..])
        in let f = (*10)
            (y:ys) = z:zs
            in let (x:_) = f y : map f ys
                in x
    = ...
    = 10
-}

