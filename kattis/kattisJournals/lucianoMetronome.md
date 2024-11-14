I found the first two problems, metronome (this) and reduplication by sorting the Kattis problems by easy.

Looking at the problem, it seemed pretty simple. It's just a division problem. Each reveloution has 4 ticks, and given the number of ticks in a song we need to find out how many full revolutions our glockenspiel whiz Milo needs to make the metronome stop right on time.

While the math part was easy, what was not as easy was the lovely way Haskell handles IO side effects and various different numbers. Especially considering how the problem requires the answer to two decimal places, and I never really liked using the `Integral` type.

First I defined my data types, which was easy and should be straightforward.
```
type Ticks = Int
type Revolutions = Double
type Solution = String
```

After that I worked on getting the input (the length of the songs in a tick), which I did by using lines to split the string, getting the head of it, and reading it to extract our int.
```
parseInput :: String -> Ticks
parseInput input = read (head (lines input)) :: Ticks
```

Doing the work involved dividing the ticks by 4, but to get our `Double` value from that I just used `fromIntegral` to achieve this.
```
doTheWork :: Ticks -> Revolutions
doTheWork ticks = fromIntegral ticks / 4.0
```

And finally showing our result. I'll admit I got lazy with this and found a `printf` function that handled it for me.
```
showResult :: Revolutions -> Solution
showResult revolutions = printf "%.2f" revolutions
```

This was straightforward, and I enjoyed using this exercise as a way to rehash handling decimal point operations in Haskell and familarizing myself again with our `showResult . doTheWork . parseInput` problem solving structure!

Also, as a note, I know that I didn't use the IO Interact within the `main :: IO()` function but I prefered to just use a straight forward approach for this.