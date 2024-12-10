import Data.List (sort)
import Data.Foldable (foldl')

type Input = (Int, [Int])
type Result = Int

main :: IO ()
main = do
    input <- getContents
    putStrLn $ solveCappella input

solveCappella :: String -> String
solveCappella = showResult . doTheWork . parseInput

parseInput :: String -> Input
parseInput input =
    let ws = words input
        n = read (head ws) :: Int
        d = read (ws !! 1) :: Int
        pitches = map read (take n (drop 2 ws)) :: [Int]
    in (d, pitches)

doTheWork :: Input -> Result
doTheWork (dVal, pitchList) =
    let sortedPitches = sort pitchList
        -- Fold through the sorted pitches to count the number of groups
        (_, totalGroups) = foldl' (\(currentStart, count) p ->
                                        case currentStart of
                                            Nothing ->
                                                (Just p, count + 1)             -- Group hasn't been started yet, start new group
                                            Just start ->
                                                if p - start > dVal
                                                    then (Just p, count + 1)    -- Existing group has exceeded difference, start new group
                                                    else (currentStart, count)  -- Difference has not been exceeded, continue this group
                                     ) (Nothing, 0) sortedPitches
    in totalGroups

showResult :: Result -> String
showResult = show
