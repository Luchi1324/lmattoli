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

## 