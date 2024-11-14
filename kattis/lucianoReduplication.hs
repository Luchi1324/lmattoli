type WordToRepeat = String
type RepeatCount = Int
type Solution = String

main :: IO ()
main = solveReduplication >>= putStrLn

solveReduplication :: IO Solution
solveReduplication = showResult . doTheWork <$> parseInput

parseInput :: IO (WordToRepeat, RepeatCount)
parseInput = do
    word <- getLine
    countStr <- getLine
    let count = read countStr :: Int
    return (word, count)

doTheWork :: (WordToRepeat, RepeatCount) -> Solution
doTheWork (word, count) = concat (replicate count word)

showResult :: Solution -> String
showResult = id
