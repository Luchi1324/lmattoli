solveSpavanac :: Int -> Int -> (Int, Int)
solveSpavanac h m
    | m >= 45   = (h, m - 45)
    | otherwise = ((h - 1) `mod` 24, 60 + m - 45)

main :: IO ()
main = do
    print "Input 10 10"
    print (solveSpavanac 10 10)