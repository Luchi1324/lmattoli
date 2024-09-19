main :: IO()
main = do
    print "average [5, 10, 15, 20] = 12.5"
    print (average [5, 10, 15, 20])
    print "duple 4 2"
    print (duple 4 2)
    print "down [1,2,3]"
    print (down [1,2,3])

average :: [Int] -> Int
average [] = 0
average (x:xs) = (x + average xs) `div` 2


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