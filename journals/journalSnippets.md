## Maybe (w/ Just & Nothing)
`Maybe` is like Swift optionals
- `Just` is a return of the successful computation of a value of type `a`
- `Nothing`is a failure or absense of this value.

Example usage:
```
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing  -- Division by zero, return Nothing
safeDivide x y = Just (x `div` y)  -- Division is valid, return Just the result
```

## Words

## Lines, and Unlines

## Flip
A flip essentially takes in a fuction and reverses the order
flip :: (a -> b -> c) -> b -> a -> c

Example
```
-- Original function
subtract :: Int -> Int -> Int
subtract x y = x - y

-- Using flip to reverse arguments
flippedSubtract :: Int -> Int -> Int
flippedSubtract = flip subtract

main :: IO ()
main = do
    print (subtract 10 5)        -- Output: 5
    print (flippedSubtract 10 5) -- Output: -5
```
