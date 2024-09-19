countDistinctCities :: [String] -> Int
countDistinctCities = undefined

main :: IO ()
main = do
    let testInput = ["boston", "cleveland", "tokyo", "bejing", "milan"]
    print (countDistinctCities testInput)
