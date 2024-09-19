import Data.List ((\\))
main :: IO ()
-- parseInput -> doTheWork -> showResult
main = interact (showResult . doTheWork . parseInput)

parseInput :: String -> [String]
parseInput s = tail (lines s)

-- containsAllOf :: [String] -> [String] -> Bool
-- containsAllOf items ["phone", "wallet", "keys"] = False

-- whatsLeft :: [String] -> [String] -> [String]
-- whatsLeft ["phone", "wallet", "keys"] items = ["wallet"]
-- use (\\)

-- Takes a list of items i have and returns list of necessities i dont have
doTheWork :: [String] -> [String]
doTheWork items = ["phone", "wallet", "keys"] \\ items

showResult :: [String] -> String
showResult [] = "Ready"
showResult xs = unlines xs