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


(NOT GOING TO BE IN EXAM, applying definitons to lazy evaulation for reduction)
Defs:
head (x:_) = x
map _ [] = []
map f (x:xs) = f x : map f xs
[] ++ ys = []
(x:xs) ++ ys = x : (xs ++ ys)

head (map (*10) ([1,2,3] ++ [4..]))
    => let xs = map (*10) ([1,2,3] ++ [4,..]) -- insert let for each subexpression not in WHNF
        in head xs

    => let ys = [1,2,3] ++ [4,..] -- insert let for each subexpression not in WHNF
        in let xs = map (*10) ys
            in head xs

    => let ys = 1 : ([2,3] ++ [4,..]) -- applied part 2 of definition of (++)
        in let xs = map (*10) ys
            in head xs

    => let xs = (1 * 10) : map (*10) ([2,3] ++ [4,..]) -- plugging in ys and applying part 2 of definition of map
        in head xs
    
    => 1 * 10 -- plugging in xs and applying definitoon of head
    => 10 -- applying multiplication
-}

exampleTake :: [Integer]
exampleTake = take 2 (tail [1,2,3,4])

{- Evaulation of exampleTake
(NOT GOING TO BE IN EXAM, applying definitons to lazy evaulation for reduction)
Defs:
tail (_:xs) = xs
take _ [] = []
take 0 _ = []
take n (x:xs) = x : take (n-1) xs

take 2 (tail [1,2,3,4])
    => let xs = tail [1,2,3,4]
        in take 2 xs

    => let xs = [2,3,4] -- we are here since [1,2,3,4] is in WHNF and we substituted the definition of tail
        in take 2 xs

    => 2 : take (2-1) [3,4] -- by plugging in xs and then applying part 3 of definition of take

    => let n = 2 - 1
        in 2 : take n [3,4] -- insert let expression with variable for unreduced sub-expression

    => let n = 1 -- reduced n by applying subtraction
        in 2 : take n [3,4]
    
    => 2 : (3 : take (1-1) [4]) -- by plugging in n and then applying part 3 of definition of take

    => let m = 1 - 1 -- insert let expression with variable for unreduced sub-expression
        in 2 : (3 : take m [4])

    => let m = 0 -- reduced n by applying subtraction
        in 2 : (3 : take m [4])
        --          take 0 [4]

    => 2 : (3 : []) -- by plugging in m and then applying part 2 of definition of take. Can't reduce any further so we're done!

-}

