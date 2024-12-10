import Data.List (permutations)
import qualified Data.Set as Set

type Pair = (Integer, Integer)
type Input = [String]
type Result = [Int]

main :: IO ()
main = do
    input <- getContents
    putStr $ solve input

solve :: String -> String
solve = showResult . doTheWork . parseInput

parseInput :: String -> Input
parseInput input = take t testCases
    where
        -- Parse the input into a list of lines
        ls = lines input
        t = read (head ls) :: Int
        testCases = tail ls

-- Process all test cases to produce a list of prime counts
doTheWork :: Input -> Result
doTheWork = map countPrimes
    where
        countPrimes :: String -> Int
        countPrimes digits = Set.size $ Set.filter isPrime $ generateNumbers digits

        -- First generates all combinations for each digit, and then for each combination generate the permutations of it
        generateNumbers :: String -> Set.Set Integer
        generateNumbers digits = Set.fromList [read p | k <- [1..length digits], comb <- combinations k digits, p <- permutations comb]

        -- Generate all combinations of size k from the list
        combinations :: Int -> [a] -> [[a]]
        combinations 0 _  = [[]]
        combinations _ [] = []
        combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [x | x <- [3,5..integerSqrt n], n `mod` x == 0]

integerSqrt :: Integer -> Integer
integerSqrt = floor . sqrt . fromIntegral

-- Convert the result list into a single string with each count on a new line
showResult :: Result -> String
showResult = unlines . map show
