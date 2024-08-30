-- functionName :: Type1 -> ReturnType
-- functionName param1 = expression
unchanged :: a -> a
unchanged x = x

changed :: Int -> Int
changed x = 2 * x

x = 5

-- Example of a function that adds two numbers (called with add 3 4)
-- functionName :: Type1 -> Type2 -> ReturnType
-- functionName param1 param2 = expression
--add :: Int -> Int -> Int
--add x y = x + y