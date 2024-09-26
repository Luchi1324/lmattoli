main :: IO()
main = do
    print "solveFilip 734 893(class)"
    -- works with [showTheWork' (doTheWork' (parseFilip' "734 893"))] in GHCI
    -- or [showTheWork' . doTheWork' . parseFilip' $ "734 893"]
    solveFilip'


solveFilip :: Int -> Int -> Int
solveFilip = undefined
-- map filipNumber . map read . words
-- then call maximum

filipNumber :: Int -> Int
filipNumber n = ones * 100 + tens * 10 + hundreds
  where
    ones     = n `mod` 10
    tens     = (n `div` 10) `mod` 10
    hundreds = (n `div` 100) `mod` 10

-- Class solution
solveFilip' = interact (showTheWork' . doTheWork' . parseFilip')
{- 
    break input string into words
    reverse each words
    interpret each word as an integer
    compare the two integers to get the larger one
    covert larger int to a string representation

    show (maximum (map read (map reverse) (words s))))
    (show . maximum . map read . map reverse . words) s
-}

parseFilip' = words

doTheWork' :: [String] -> Int
doTheWork' = maximum . map read . map reverse

showTheWork' = show
