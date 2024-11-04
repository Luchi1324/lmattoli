import Data.List ((\\))
main :: IO ()
-- parseInput -> doTheWork -> showResult
main = interact (showResult . doTheWork . parseInput)

parseInput :: String -> [String]
parseInput s = tail (lines s)

-- Takes a list of items i have and returns list of necessities i dont have
doTheWork :: [String] -> [String]
doTheWork items = ["phone", "wallet", "keys"] \\ items

showResult :: [String] -> String
showResult [] = "Ready"
showResult xs = unlines xs