import GHC.RTS.Flags (RTSFlags(concurrentFlags))
import Data.List (nub)
-- Type specfiying
type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

-- Funcs for digits and a 'blank' 0
digits :: [Char]
digits = ['1'..'9']
blank :: Digit -> Bool
blank = (== '0')

-- Solving the sudoku puzzle
solve :: Grid -> [Grid]
solve = filter valid . completions

-- Getting every possible completion, and then finding out which of those is valid
completions :: Grid -> [Grid]
completions = expand . choices

valid :: Grid -> Bool
valid g =   all nodups (boxs g) &&       -- all is a prelude function (all :: (a -> Bool) -> [a] -> Bool), that checks if something applies to all elements in a list
            all nodups (cols g) &&
            all nodups (boxs g)
-- No duplicates to help with valid
nodups :: [Digit] -> Bool
nodups xs = length (nub xs) == length xs -- Nub removes duplicates from a list
nodups [] = True
nodups (x:xs) = not (x `elem` xs) && nodups xs

rows :: Matrix a -> [Row a]
rows = id

cols            :: Matrix a -> [Row a]
cols [xs]       = [[x] | x <- xs] -- xs is a single row
cols (xs:xss)   = zipWith (:) xs (cols xss)

boxs :: Matrix a -> [Row a]
boxs = map ungroup . ungroup . map cols . group . map group

ungroup = concat
group []           = []
group (x:y:z:xs)   = [x,y,z] : group xs


choices :: Grid -> Matrix [Digit]
choices = map (map choice)
    where 
        choice d = if blank d then digits else [d]
expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

-- Using the cartesian product to produce a completed grid
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = 
    [x:ys | x <- xs, ys <- cp xss]

