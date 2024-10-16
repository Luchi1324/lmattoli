
-- IO with the do notaton
f :: IO()
f = do
    x <- readLn
    y <- readLn
    print $ x * y -- same as print (x * y)

-- Same as above, written without do notation
g :: IO ()
g = readLn >>= (\x -> 
      readLn >>= (\y -> 
        print (x * y)))

-- >>= Bind operator (output of one computation is the input of the next)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- binding [1,2,3] to this anonymous fuction gives us [2,4,6]
xs = [1,2,3] >>= (\x -> [x * 2])

-- With the do, <- binds a value to a variable, binding y*2 to each value in the list
ys = do
    y <- [1,2,3]
    [y*2]


-- Flip function (flip :: (a -> b -> c) -> b -> a -> cs)

-- Haskell Currying
-- Currying a function allows us to neatly type it without breackets
{-
For example
multiply :: Int -> Int -> Int
multiply x y = x * y
Has a type of Int -> Int -> Int but is actually interpreted as Int -> (Int -> Int)
It takes in an Int and returns a function (Int -> Int)
this returned function takes in another Int and finally returns our final Int
-}
as = zip [1..] [2..]

bs = map (uncurry(+)) as