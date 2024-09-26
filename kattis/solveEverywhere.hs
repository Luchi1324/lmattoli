import Data.List (nub)


type City = String
type TestCase = [City]     -- [String]
type Problem = [TestCase]  -- [[String]]
type Solution = [Int]

solveEverywhere :: String -> String
solveEverywhere = showResult . doTheWork . parseInput

parseInput :: String -> Problem
parseInput = readCases . tail . lines        -- lines takes in a string and splits it into a list of strings based on \n
    where
        readCases :: [String] -> [[String]]
        readCases [] = []
        readCases (x:xs) = let n = read x in
            take n xs : readCases (drop n xs)

doTheWork :: Problem -> [Int]
doTheWork = map solveOne
    where
        solveOne :: [String] -> Int
        solveOne cities = length (nub cities)

showResult :: Solution -> String
showResult = unlines . map show              -- unlines takes a list of strings and concat them into a single string, inserting a \n between each original string


