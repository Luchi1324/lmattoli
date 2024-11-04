
solveClassFieldTrip :: String -> String -> String
-- regular pattern matching
solveClassFieldTrip [] ys = ys
solveClassFieldTrip xs [] = xs
solveClassFieldTrip (x:xs) (y:ys)
    | x < y     = x : solveClassFieldTrip xs (y:ys)
    | otherwise = y : solveClassFieldTrip (x:xs) ys

-- using head and tail instead of (x:xs)
solveClassFieldTrip' :: String -> String -> String
solveClassFieldTrip' xs ys
    | null xs = ys
    | null ys = xs
    | head xs < head ys = head xs : solveClassFieldTrip' (tail xs) ys
    | otherwise         = head ys : solveClassFieldTrip' xs (tail ys)

main :: IO ()
main = do
    print "Input: 'ahjmnoy' and 'acijjkll'"
    print (solveClassFieldTrip "ahjmnoy" "acijjkll")
