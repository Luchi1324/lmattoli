solveSpavanac :: Int -> Int -> (Int, Int)
solveSpavanac h m
    | m >= 45   = (h, m - 45)
    | otherwise = ((h - 1) `mod` 24, 60 + m - 45)

-- Rewriting it in the 'showResult . doTheWork . parseInput' structure
type Hours = Int
type Minutes = Int
type Time = (Hours, Minutes)

type Input = String
type Output = String
{- 
type Input = String
type Output = String

type Time = (Int, Int)
type Problem = Time
-}

solveSpavanac' :: Input -> Output
solveSpavanac' = showResult . doTheWork . parseInput

parseInput :: Input -> Time
parseInput str = let [hours, minutes] = map (read::String -> Int) (words str) in (hours, minutes)

doTheWork :: Time -> Time
doTheWork (hour, minutes) = do
    let minutes = hour * 60 + minutes
    let minutes_translated = minutes - 45
    let adjusted_minutes = if minutes_translated >= 0 then minutes_translated else minutes_translated + 24 * 60
    (adjusted_minutes `div` 60, adjusted_minutes `mod` 60)

showResult :: Time -> Output
showResult (hours, minutes) = show hours ++ " " ++ show minutes

main :: IO ()
main = do
    print "Input 10 10 w/ class structure"
    print (solveSpavanac' "10 10")