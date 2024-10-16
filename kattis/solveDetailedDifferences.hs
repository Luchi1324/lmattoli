main :: IO ()
main = interact (report . solve . parse)

type TestCase = (String, String)
type Diff = String
type Result = (TestCase, Diff)

-- words["3", "ATCCGC....", "GTCC....", "abcd...", "..."]
-- tail["ATCCGC....", "GTCC....", "abcd...", "..."]
parse :: String -> [TestCase]
parse = readCases . tail . words
    where
        readCases [] = [] -- no more lines/test cases
        readCases (s1 : s2 : ss) = (s1, s2) : readCases ss
        -- Since we always have a pair, we use pattern matching to create a pair from two elements

solve :: [TestCase] -> [Result]
solve = map solve1
    where
        -- solve1 (s1, s2) = zipWith diff1 (s1, s2)
        solve1 :: TestCase -> Result
        solve1 cs = (cs, uncurry (zipWith diff1) cs)
        diff1 :: Char -> Char -> Char
        diff1 c1 c2 = if c1 == c2 then '.' else '*'

report :: [Result] -> String
report = unlines . concatMap report1 -- Applies functions across a list, then concats the resulting list into a single list
    where
        report1 :: Result -> [String]
        report1 ((s1, s2), diff) = [s1, s2, diff]
        --report1 res = [fst (fst r), snd (fst r), snd 4]