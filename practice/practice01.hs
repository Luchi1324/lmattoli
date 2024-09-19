import Data.Char (isAlpha, toLower)

-- Problem 1
{-
  public Object elemAt (int n, List<Object> xs)
-}
elemAt :: Int -> [a] -> a
elemAt n [] = error "elemAt: empty list"
elemAt 0 (x:xs) = x
elemAt n (x:xs) = elemAt (n - 1) xs
-- Class solutions:
-- if, just recursive
-- elemAt i xs = 
--  if null xs
--    then error "elemAt: empty list"
--  if i > 0 
--    then elemAt (i-1) (tail xs) 
--    else head xs
-- guarded statement
-- elemAt i xs
--  | null xs = error "elemAt: empty list"
--  | i > 0 = elemAt (i-1) (tail xs)
--  | otherwise [this just means true] = head xs
-- pattern matching
-- elemAt i [] = error "elemAt: empty list"
-- elemAt 0 xs = head xs
-- elemAt i xs = elemAt (i-1) (tail xs)
-- pattern matching + guard
-- elemAt _ [] = error "elemAt: empty list"
-- elemAt i (x:xs)
--  | i > 0 = elemtAt (i-1) xs
--  | otherwise = head xs
-- smart version
-- elemAt i xs = xs !! i
-- smarter version (point free form)
-- elemAt :: Int -> [a] -> a
-- elemAt = flip (!!)

-- Problem 2
isPalindrome :: String -> Bool
isPalindrome str = isSame (cleanString str) (reverseString (cleanString str))

cleanString :: String -> String
cleanString [] = []
cleanString (x:xs)
-- I remember the 'guard' structure from some functional languages in TPL, works in Haskell too
  | isAlpha x = toLower x : cleanString xs
  | otherwise = cleanString xs

reverseString :: String -> String
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]

isSame :: String -> String -> Bool
isSame [] [] = True
isSame (x:xs) (y:ys) = (x == y) && isSame xs ys
isSame _ _ = False

-- Problem 3
partition :: [a] -> ([a], [a])
partition xs = partitionHelper xs (length xs `div` 2)

partitionHelper :: [a] -> Int -> ([a], [a])
partitionHelper [] _ = ([], [])
partitionHelper (x:xs) 0 = ([], x:xs)
partitionHelper (x:xs) n = (x : left, right)
  where (left, right) = partitionHelper xs (n - 1)

-- Problem 4
showDate :: (Int, Int, Int) -> String
showDate (day, month, year) = show day ++ suffix day ++ " " ++ monthName month ++ ", " ++ show year

suffix :: Int -> String
suffix day
  | day `elem` [11, 12, 13] = "th"
  | lastDigit == 1 = "st"
  | lastDigit == 2 = "nd"
  | lastDigit == 3 = "rd"
  | otherwise = "th"
  where lastDigit = day `mod` 10

monthName :: Int -> String
monthName 1  = "January"
monthName 2  = "February"
monthName 3  = "March"
monthName 4  = "April"
monthName 5  = "May"
monthName 6  = "June"
monthName 7  = "July"
monthName 8  = "August"
monthName 9  = "September"
monthName 10 = "October"
monthName 11 = "November"
monthName 12 = "December"

-- Problem 5
slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice i k (x:xs)
  | i > 0 = slice (i - 1) (k - 1) xs
  | k > 0 = x : slice 0 (k - 1) xs
  | otherwise = []

-- Problem 6
rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate _ [] = []
rotate n (x:xs) = rotate (n - 1) xs ++ [x]

-- Problem 7
solveFindingAnA :: String -> String
solveFindingAnA [] = []
solveFindingAnA (x:xs)
  | x == 'a'  = x:xs
  | otherwise = solveFindingAnA xs

-- Problem 8
solveCold :: [Int] -> Int
solveCold [] = 0  -- Base case: if the list is empty, return 0
solveCold (x:xs)
  | x < 0     = 1 + solveCold xs  -- If the current temperature is below zero, add 1 to the count
  | otherwise = solveCold xs      -- Otherwise, just move to the next temperature

-- Problem 9
solveWhichIsGreater :: Int -> Int -> Int
solveWhichIsGreater a b
  | a > b     = 1
  | otherwise = 0

-- Problem 10
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isVowelWithY :: Char -> Bool
isVowelWithY c = c `elem` "aeiouy"

solveIsYAVowel :: String -> (Int, Int)
solveIsYAVowel [] = (0, 0)
solveIsYAVowel (x:xs)
  | isVowel x      = let (a, b) = solveIsYAVowel xs in (a + 1, b + 1)
  | isVowelWithY x = let (a, b) = solveIsYAVowel xs in (a, b + 1)
  | otherwise      = solveIsYAVowel xs

-- Problem 11
solveDetailedDifferences :: [(String, String)] -> [String]
solveDetailedDifferences [] = []
solveDetailedDifferences ((str1, str2):xs) =
  str1 : str2 : compareStrings str1 str2 : "" : solveDetailedDifferences xs

compareStrings :: String -> String -> String
compareStrings [] [] = []
compareStrings (x:xs) (y:ys)
  | x == y    = '.' : compareStrings xs ys
  | otherwise = '*' : compareStrings xs ys

-- Problem 12
solveKeysPhoneWallet :: [String] -> [String]
solveKeysPhoneWallet items = missingItemsHelper ["keys", "phone", "wallet"] items

missingItemsHelper :: [String] -> [String] -> [String]
missingItemsHelper [] _ = []
missingItemsHelper (x:xs) items
  | x `notElemRecursive` items = x : missingItemsHelper xs items
  | otherwise                  = missingItemsHelper xs items

notElemRecursive :: String -> [String] -> Bool
notElemRecursive _ [] = True
notElemRecursive y (x:xs)
  | y == x    = False
  | otherwise = notElemRecursive y xs