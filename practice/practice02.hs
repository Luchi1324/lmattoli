
-- Question 1
--cubic :: Int -> Int -> Int -> Int -> Int -> Int
cubic :: Num a => a -> a -> a -> a -> a -> a
cubic a b c d x = a*x^3 + b*x^2 + c*x + d

-- Question 2
lastElem :: [a] -> a
lastElem [x] = x
lastElem (_:xs) = lastElem xs
-- lastElem = head . reverse (point free)

-- Question 3
applyToEach :: (a -> b) -> [a] -> [b]
-- pattern matching\

--applyToEach _ [] = []
--applyToEach f (x:xs) = f x : applyToEach f xs
applyToEach f xs
    | null xs = []
    | otherwise = f (head xs) : applyToEach f (tail xs)


main :: IO()
main = do
    print "Problem 1: "
    print (cubic 2 0 (-6) 1 10)
    print "Problem 2: "
    print (lastElem [1,2,3,4,5])
    print "Problem 3: "
    print (applyToEach (+ 1) [1,2,3,4,5])