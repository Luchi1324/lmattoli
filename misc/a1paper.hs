
main = interact (report . solve . parse)


-- "4\n1 0 5\n"
-- words -> ["4", "1", "0", "5"] (Words breaks up String into [String])
-- tail -> ["1", "0", "5"]
parse :: String -> [Integer]
parse = map read . tail . words

report :: Maybe Double -> String    -- Maybe is like Swift optionals
report (Just x) = show x            -- Just is a return of the successful computation of a value of type a ...
report Nothing = "impossible"       -- ... and Nothing is a failure or absense of this value.

-- From a list of counts of papers at each size, determine how much tape we need (or if we fail)
solve :: [Integer] -> Maybe Double
solve = fmap (tapelen . levelup) . papersToUse

-- From the list of papers we have, determine how many papers
-- we will use of size, or determine that we can't do it.
papersToUse :: [Integer] -> Maybe [Integer]
papersToUse = papersHelper 2
    where 
        papersHelper _ [] = Nothing
        papersHelper need (have:haves)
            | have >= need = Just [need]
            | otherwise = (have:) <$> papersHelper (2 * (need-have)) haves




-- How many edges at each size need to be taped?
levelup :: [Integer] -> [Integer]
levelup [x] = [x `div` 2]
levelup (x:xs) = (x + y) `div` 2 : y : ys
    where
        (y:ys) = levelup xs

-- From the edge counts at each size, compute the total length of tape requied
tapelen :: [Integer] -> Double
tapelen edgeCounts = sum $ zipWith ((*) . fromIntegral) edgeCounts edgeLengths

-- list of lengths for all the long edges at each paper size
-- A2 is 2**(-3/4), A3 is 2**(-5/4), A4 is 2**(-7/4), and so forth
-- Ai is 2**(-(2*i-1)/4)
edgeLengths :: [Double]
edgeLengths = [ 2**(- (k / 4)) | k <- [3,5..]]