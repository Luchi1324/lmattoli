import Data.List (intercalate)

type Pair = (Integer, Integer)
type Input = [Pair]
type Result = [Integer]

main :: IO ()
main = do
    input <- getContents
    putStr $ solve input

solve :: String -> String
solve = showResult . doTheWork . parseInput

parseInput :: String -> Input
parseInput = map parseLine . lines
  where
    -- For each line, parse the two integers into a pair
    parseLine :: String -> Pair
    parseLine line =
        let [a, b] = map read (words line) :: [Integer]
        in (a, b)

doTheWork :: Input -> Result
doTheWork = map computeDifference
  where
    -- For each pair, compute the absolute difference between them
    computeDifference :: Pair -> Integer
    computeDifference (a, b) = abs (a - b)

showResult :: Result -> String
-- intercalate is a neat function that concatenates each result and inserts a newline between them
showResult = intercalate "\n" . map show
