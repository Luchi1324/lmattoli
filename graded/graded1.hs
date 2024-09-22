main :: IO()
main = do
    print "average [5, 10, 15, 20] = 12.5"
    print (average' [5, 10, 15, 20])
    print "duple 4 2"
    print (duple 4 2)
    print "down [1,2,3]"
    print (down [1,2,3])
    print "swapper \'a\' \'d\'  \"abcd\""
    print (swapper' 'a' 'd'  "abcd")

average :: [Int] -> Int
average [] = 0
average (x:xs) = (x + average xs) `div` 2
-- Post assignment work
average' :: Fractional a => [a] -> a
average' [] = 0
average' xs = total / count
  where
    total = sum xs
    count = fromIntegral (length xs)


duple :: Int -> a -> [a]
duple n x
    | n <= 0    = []
    | otherwise = x : duple (n - 1) x


down :: [a] -> [[a]]
down xs
    | null xs = []
    | otherwise = downHelper (head xs) : down (tail xs)

downHelper :: a -> [a]
downHelper a = [a]

swapper :: a -> a -> [a] -> [a]
swapper = undefined
-- Post assignment work
swapper' :: Eq a => a -> a -> [a] -> [a]
swapper' y1 y2 xs = [if x == y1 then y2 else if x == y2 then y1 else x | x <- xs]