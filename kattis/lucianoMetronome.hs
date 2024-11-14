import Text.Printf (printf)

type Ticks = Int
type Revolutions = Double
type Solution = String

main :: IO ()
main = do
    input <- getLine
    putStrLn $ solveMetronome input

solveMetronome :: String -> String
solveMetronome = showResult . doTheWork . parseInput

parseInput :: String -> Ticks
parseInput input = read (head (lines input)) :: Ticks

doTheWork :: Ticks -> Revolutions
doTheWork ticks = fromIntegral ticks / 4.0

showResult :: Revolutions -> Solution
showResult revolutions = printf "%.2f" revolutions
