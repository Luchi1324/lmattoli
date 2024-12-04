# An Industrial Spy
## Start
I saw this and it seemed interesting as a medium problem that was not too complex. However, once I saw it involved calculating prime numbers, I was immediately intrigued. The problem involed an industrial spy needing to figure out how many unique prime numbers could be formed by rearranging any subset of given digits. The main challenge here was finding out how to generate all possible combinations of said digits and verifying their primality in a way that doesn't eat up all the RAM and CPU usage in my laptop.

## Data Types
Pretty self explanatory, we can define our `input` (test case of digits) as a list of `String`s and our output (count of unique prime numbers) as a list of `Int`s.
```haskell
type Input = [String]
type Result = [Int]
```

## Parse Input
This was pretty simple. The first line specifies the number of test cases, and each test case has a string of digits.
```haskell
parseInput :: String -> Input
parseInput input = take t testCases
    where
        -- Parse the input into a list of lines
        ls = lines input
        t = read (head ls) :: Int
        testCases = tail ls
```

## Do The Work
### Generating all of the combinations
To first approach the problem, I needed to generate all of the possible unique numbers from the digits given. Some digging around, and I got it working by using Haskell's `Set.fromList` to store each number in a set and ensure that they were all unique. I also `permutations` from Haskell's `Data.List` module to generate all possible orderings from a combination, and wrote a helper function `combinations` to generate said combinations.
```haskell
-- Generate all unique numbers from the digits by rearranging any subset
generateNumbers :: String -> Set.Set Integer
generateNumbers digits = Set.fromList [read p | k <- [1..length digits], comb <- combinations k digits, p <- permutations comb]

-- First generates all combinations for each digit, and then for each combination generate the permutations of it
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs
```

### Finding how many of those combinations are prime numbers
Now came the fun part. So we all know what a prime number is, a number only divisible by 1 and itself. Immediately we can determine that if a number is less than 2, equal to 2, or an even number, then it is not a prime number. Then we build up a list of odd numbers from 3 to `n` and filter out how many of them are divisible by `n` (the `n ``mod`` x == 0` condition). If there are no divisors, the number is prime. At first I tried a more straight forward approach.
```haskell
isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [x | x <- [3,5..floor (sqrt (fromIntegral n))], n `mod` x == 0]
```

However, this would be very ineffient at larger numbers (up to 10<sup>7</sup>). Additionally, I noticed that it would not count some prime numbers accurately. After looking at it, I realized that the way I converted `n` to a floating point number was causing some precision issues. I chose this approach initially for ease, but after realizing the limitations of floating point operations (thanks comp org & arch!), I decided to write a small function to calculate the root as an integer seperately from the main `isPrime` function with `integerSqrt` to avoid these issues. That seemed to fix the issue.
```haskell
isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [x | x <- [3,5..integerSqrt n], n `mod` x == 0]

integerSqrt :: Integer -> Integer
integerSqrt = floor . sqrt . fromIntegral
```

## Show Result
In order to show our result, we just need to `unlines` our `Result` and show each of them.
```haskell
showResult :: Result -> String
showResult = unlines . map show
```

## Final Thoughts
While the prime number took me a bit to figure out, the rest of the problem was not too hard to work with and I had a lot of fun working with this! It reminded me of why `fromIntegral` is pretty useful, and also why managing Integers and floating point units carefully is important. I'll admit, with list comprehensions and things such as pattern guarding, it seems like making this in Haskell was easier than if I were to try to approach it in another language.