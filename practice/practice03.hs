import Distribution.Compat.CharParsing (endBy)
-- Problem 1
sq :: Num a => a -> a
sq a = a * a

-- Problem 2
sumInts :: Int -> Int -> Int
--sumInts a b = sum [a..b]
sumInts start end
    | start == end = start
    | otherwise = start + sumInts (start + 1) end

-- Problem 3
sumSquares :: Int -> Int -> Int
sumSquares a b = sum (map sq [a..b])
--sumSquares a b = sum $ map sq [a..b]

-- Problem 4
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b = sum (map f [a..b])

-- Problem 5
hoSumSquares :: Int -> Int -> Int
--hoSumSquares a b = higherOrderSum sq a b
hoSumSquares = higherOrderSum sq

-- Problem 6
hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum (*1)

-- Problem 7
hoSequenceApplication :: (Int -> Int -> Int) -> Int -> Int -> Int
hoSequenceApplication f start end
    | start == end = end
    | otherwise = start `f` hoSequenceApplication f (start + 1) end
-- ` takes in operator and makes it as a function

-- Problem 8
hoFactorial :: Int
hoFactorial = undefined


main :: IO()
main = do
    print "Problem 1: sq 5"
    print (sq 5)
    print "Problem 2: sumInts 1 3"
    print (sumInts 1 3)
    print "Problem 3: sumSquares 1 3"
    print (sumSquares 1 3)
    print "Problem 4: higherOrderSum *2 1 3"
    print (higherOrderSum (*2) 1 3)
    print "Problem 5: hoSumSquare 1 3"
    print (hoSumSquares 1 3)
    print "Problem 6: hoSumInts 1 3"
    print (hoSumInts 1 3)
