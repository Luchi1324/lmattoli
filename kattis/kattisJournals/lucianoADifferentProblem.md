# A Different Problem
## Start
I have to admit I am very very packed with finals week and finding this medium problem was like finding a golden goose. It's ranked medium, but the problem itself is very straightforward. Given a list of a pair of non-negative integers, we just need to compute and print the absolute difference between them. So many of the medium problems require very intricate input parsing or very convoluted mathematical operations, and this was a nice break.

## Data Types
Here I just wrapped pair as a tuple of `Integers`, define our `input` as a list of `pair`s and the result as a list of `Integer`s.
```haskell
type Pair = (Integer, Integer)
type Input = [Pair]
type Result = [Integer]
```

I chose `Integer` as opposed to `Int` as the first test case had some very larger numbers and I wanted to avoid overflow errors.

## Parse Input
Since each line contains two integers seperated by a space, and the input is just interupted by the end of the file, we can just read each line and form a list of `Pair`s by splitting them into two integers.
```haskell
parseInput :: String -> Input
parseInput = map parseLine . lines
  where
    -- For each line, parse the two integers into a pair
    parseLine :: String -> Pair
    parseLine line =
        let [a, b] = map read (words line) :: [Integer]
        in (a, b)
```

## Do The Work
From our `input`, we can just `map` a helper function to compute the absolute difference by taking in a `Pair`, calculating the absolute difference between them, and returning this value.
```
doTheWork :: Input -> Result
doTheWork = map computeDifference
  where
    computeDifference :: Pair -> Integer
    computeDifference (a, b) = abs (a - b)
```

## Show Result
We have to produce a line for each result, but from this I learnt about Haskell's `intercalate` function. It takes in a list and seperates them while inserting a character between. So the showResult function technically returns a single string, but it prints it with a newline to print them in multiple lines. Pretty neat.
```haskell
showResult :: Result -> String
-- intercalate is a neat function that concatenates each result and inserts a newline between them
showResult = intercalate "\n" . map show
```

## Final Thoughts
This was quick and not too hard to implement, plus I got to learn about another powerfull Haskell function with `intercalate`. Honestly, given how intense everything else is, this problem being a medium felt like a godsend.