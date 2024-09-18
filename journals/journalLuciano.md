## Practice 1 Problems
I worked today on the practice 1 problems. Unfortunately I wasn't able to make the class due to some personal issues I had to handle.
Trying to approach each problem purely recursively in a functional manner was a bit of a challenge, but I refered to some of my old codes in TPL since a lot of the languages were functional ones. 
After that, and using a mixture of the textbook/chat to help me find how the guard and other structures worked in Haskell, I was able to get them working. 
(A helper function I made for the palindrome problem)
```
cleanString :: String -> String
cleanString [] = []
cleanString (x:xs)
-- I remember the 'guard' structure from some functional languages in TPL, works in Haskell too
  | isAlpha x = toLower x : cleanString xs
  | otherwise = cleanString xs
```
They all passed the test cases provided in the assignment. I used chat more for the first two but after I used it for that, I found that I was able to work with it better for the rest of the problems. I did have to use chat a bit more for the DetailedDifferences, as the wording of the question confused me a bit.

## Chapter 2

## Chapter 3-4

## Chapter 5-6 (Higher Order Functions)

## Practice 2 Problems
The pratice 2 problems weren't that bad. The in class ones, especially the applyToEach helped me understand better how Haskell handles lists. However, the Kattis problems proved to be a bit more difficult.

## Haskell Lists (9/16/2024)
We can treat them as a singly linked list. I can already see it in how Haskell has worked with it so far (the whole (x:xs) or head tail structure). So functions that work with lists are considered recursive, higher order types (takes in a function as an argument).

In Bird, every list of type a takes either
- Undefined `undefined :: [a]`
- Empty `[] :: [a]`
- A list of the form `x:xs` where `x :: a` and `xs :: [a]`

Some example of list functions (head, tail, last, init, null, (:, ++), map, filter, zipWith, zip, DataList: concat, takeWhile, take, nub, elemIndex)

### Enumeration
There's lots of existing notation in Haskell for enumerating lists of integers. For example
`[1..5]` Note that it's inclusive in the end
`[1,3..15]`
`[1..]`
`['A'..'Z']`

### Comprehensions
Think of it like Set Comprehension notation ( {n^2 : n e Z+} )
So `[n^2 | x <- [1..]]` produces `[1, 4, 9, 16..]`
This can be used to define common functions for lists. For example,
`map f xs = [f x | x <- xs]`
However in Haskell, this works the other way around. List comprehensions are translated into equivalent definitions in terms of map and concat. `[e | Q1, Q2] = concat [[e | Q2] | Q1]`

Example of map toUpper for a list of words (since map toUpper only works on one 'word')
```
words = ["word1", "word2"]
allToUpper (x:xs) = map toUpper x : allToUpper xs
allToUpper [] = []
```
Also `map (map toUpper) words`

## Practice 3 Problems (9/18/2024)
The first few problems weren't too bad. There was a lot of built in functions that i didn't know fully about but could guess that made it a lot easier. For example...
```
sumInts :: Int -> Int -> Int
sumInts a b = sum [a..b]
sumInts start end
    | start == end = start
    | otherwise = start + sumInts (start + 1) end
```
Both achieve the same result but one uses the built in sum and enumaration function to do it, and the other uses a recursive approach to add each number to its next counterpart and stop when the last addition is equal to the last number. So it achieves it like a list without making one.

Problem7 was a bit confusing, where I understood what it wanted but I was not too sure about how to achieve it. However thanks to my partner i was able to get it working and I learnt about using backticks to create inflix expressions. I will be using these a lot in the future as my head already struggles a lot with Haskell as is.
```
hoSequenceApplication :: (Int -> Int -> Int) -> Int -> Int -> Int
hoSequenceApplication f start end
    | start == end = end
    | otherwise = start `f` hoSequenceApplication f (start + 1) end
```
The filip was a bit puzzling. I work better with more explicit languages where I can see what values are being manipulated/taken in. So a language like Haskell that uses a lot of implicit passing and with how it joins functions is something that I naturally struggle to use.
`(show . maximum . map read . map reverse . words) s` Like I get how this works and it makes sense, but just chaining all of them together or even getting to that point makes my head hurt.
