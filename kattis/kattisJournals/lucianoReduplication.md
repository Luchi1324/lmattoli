Like metronome, this was a really simple problem. We take in a word, the number of times we need to repeat the word, and output that word repeated n amount of times.

Again, since I absolutely love Haskell as a language, most of my effort was spent working around with how Haskell handles IO operations and fitting in in our `showResult . doTheWork . parseInput` structure. It took me a while since I had initial issues where it would break after processing one test case, but after some tweaking it worked just fine. Mostly a result of my limitations rather than Haskells.

So first I defined my data types, which was straightforward again.
```
type WordToRepeat = String
type RepeatCount = Int
type Solution = String
```

Parsing input took me a while to get working, since I wasn't able to get the dual input working as Kattis wanted me to. But playing around with `IO`s and `getLine`s, I was able to get it working like so.
```
parseInput :: IO (WordToRepeat, RepeatCount)
parseInput = do
    word <- getLine
    countStr <- getLine
    let count = read countStr :: Int
    return (word, count)
```

Haskell has a lovely replicate function that does exactly what I need for this problem, so to avoid reinventing the wheel I just concatenate the result of it. And since the concatenation handles a lot of the string, I just used a basic ID for showResult.
```
doTheWork :: (WordToRepeat, RepeatCount) -> Solution
doTheWork (word, count) = concat (replicate count word)

showResult :: Solution -> String
showResult = id
```

I got to show off some of the functor stuff we learnt for exam 2 in my solve and main functions.
```
main :: IO ()
main = solveReduplication >>= putStrLn

solveReduplication :: IO Solution
solveReduplication = showResult . doTheWork <$> parseInput
```

Like metronome, this was a nice refresher and I got to practice some of the functor stuff we learnt, including the bind `>>=` operator.