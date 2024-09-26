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
In Haskell, every wellformed expression has a wellformed type. Each of these wellformed expressions has a value. So within GHCi it
- First checks if the expression is syntactically correct
- After that, it infers a type for the expression or checks the type provided for it
- Provided that it is well-typed, GHCi then evaluates the expression by reducing it to its simpliest possible form
- If it is printable, GHCi then prints it at the terminal
### A session with GHCi
The command `:type expr` returns the type of an expression. For example, `:type 3 + 4` will output `3+4 :: Num a => a`. Same result if called on map, `map :: (a -> b) -> [a] -> [b]`
### Names and operators
A script is a collection of names and their definitions. Function and value names begin with a lowercase letter, with an exception for data constructors. Types (`Int`), type classes (`Num`), and modules (`Prelude` or `Data.Char`) begin with uppercase letters.

An operator is a special kind of function name that appears between its arguments, such as `+` or `++`. Operator names begin with a symbol. Any non-symbolic functions can be converted into an operator by enclosing it in backquotes, and likewise any operator can be converted to a prefix name by enclosing it in parentheses.
- `3 + 4` is the same as `(+) 3 4`
- `div 3 4` is the same as `3 ``div`` 4`
Operators do have different levels of precedence (binding power).
- `3 * 4 + 2` means `(3 * 4) + 2`
- `xs ++ yss !! 3` means `xs ++ (yss !! 3)`
Operators with the same level of precedence usually have an order of association, right or left. The usual arithmetic operators are left associative.
- `3 - 4 - 2` is `(3 - 4) - 2`
- `3 / 4 * 5` is `(3 / 4) * 5`
Functional application, which is higher than any other operator, follows this logic.
- `eee bah gum * 2` is `((eee bah) gum) * 2`
Some operators do associate to the right.
- `(a -> b) -> [a] -> [b]` is `(a -> b) -> ([a] -> [b])`
- `x ^ y ^ z` is `x ^ (y ^ z)`
- `eee . bah . gum` is `eee . (bah . gum)`
Sections and lambda expressions help to simplify thing such as helper functions. For example, sections are a device that make it so the arguments of an operator are included along with the operator.
- `(+1) n = n+1`
- `(<0) n = n>0`
- `(1/) x = 1/x`
One caveat is that (-1) is not the section that subtracts one. It's just the number, since Haskell uses the minus sign for both the binary operation of subtraction and as a prefix that denotes negative numbers.
Lambda expressions are used like `\n -> 2 * n + 1`. This can be read as 'the function of n which returns 2*n + 1.
### Evaluation
Haskell will always evaluate an expression by reducing it to its simplist possible form. For example, the following definition
```
sqr :: Integer -> Integer
sqr x = x * x
```
will be evaulated in one of two ways. With `sqr (3 + 4)`, this is done by either by doing `3+4` first or applying the definition of `sqr` first.
```
sqr (3+4)           sqr (3+4)
= sqr 7             = let x = 3+4 in x*x
= let x = 7 in x*x  = let x = 7 in x*x
= 7*7               = 7*7
= 49                = 49

```
The leftmost method is innermost reduction, or eager evaulation. The rightmost is outermost reduction, or lazy evaluation. Eager evaulation always evaulates the arguments first before applying the function, and lazy evaulation the definition of a function is installed at once and only when they are needed are the arguments to the function evaluated. For example, in a function that returns the first element of a pair (where the pair is calling `sqr` for each value), eager evaluation will compute the value `sqr 2` where in lazy evaluation it is not since the value is not needed.

This is also seen in an 'infinity` evaluation
```
three infinity            three infinity
= three (1+infinity)      = let x = infinity in 3
= three (1+(1+infinity))  = 3
= ...
```
and another example for using a recursive function such as factorials.
```
factorial 3       factorial 3
= fact (3,1)      = fact (3,1)
= fact (3-1,3*1)  = fact (3-1,3*1)
= fact (2,3)      = fact (2-1,2*(3*1))
= fact (2-1,2*3)  = fact (1-1,1*(2*(3*1)))
= fact (1,6)      = 1*(2*(3*1))
= fact (1-1,1*6)  = 1*(2*3)
= fact (0,6)      = 1*6
= 6               = 6
```
### Types and type classes
Types for a function are always written as such.
`func :: Int -> [a] -> [a]`
For applicatons that seem either too specific or broad,
- `(+) :: Int -> Int -> Int`
- `(+) :: a -> a -> a`
this is where type classes come into play.
- `(+) :: Num a => a -> a -> a`
This declaration asserst that `(+)` is of the type `a -> a -> a` for any number type a. These type classes, like `Num`, has a collection of name methods like our `(+)`. Type classes then can provide for overloaded functions (same name, different definition).

Using Eq as a example to illustrate type classes. It has an equality test `(==)` and an inequality.
```
class Eq a where
(==),(/=) :: a -> a -> Bool
x /= y = not (x == y)
```
There is a default definition for `(/=)` so we would have to provide a definition for `(==)`. We can add to type classes by defining instances, such as
```
instance Eq Bool where
x == y = if x then y else not y
```
### Printing values
In GHCi
```
ghci> "Hello ++"\n"++ "young" ++"\n"++ "lovers"
"Hello\nyoung\nlovers"
```
Haskell doesn't print it newLine because of how Haskell prints. It applies show to evaluated expressions to produce strings that look exactly like v.
- `show 42 = "42"`
- `show 'a' = "'a'"`
- `show "hello\n" = "\"hello\\n\""`
We use `putStrLn :: String -> IO ()`. 

`IO a` is a very special type, where it allows for input-output computations that have some interaction wiht the outside world and return a value of type a.

Within a .hs file, we can define a main block to run these operations.
```
main :: IO ()
main = do
    putStrLn "Input a text to print:";
    text <- getLine;
    print text;
```
### Modules
We can incorporate functions into other scripts by turning it into a module.
`module CommonWords (commonWords) where commonWords :: Int -> String -> String`

## Chapter 3-4 (Numbers & Lists)
### The type class Num and other numeric type classes
There are many different kinds of numbers in Haskell. You have:
- Int: limited precision integers in the range [-2^29, 2^29). Overflow isn't detected.
- Integer: arbitrary-precision integers
- Rational: arbitrary-precision rational numbers
- Float: single-precision floating-point numbers
- Double: double-precision floating-point numbers
- Complex: complex numbers (in Data.Complex)

All of these numbers are an instance of the type class `Num`. Since `Num` is a subclass of `Eq` and `Show`, every number can be printed and compared for equality. Any can be added to, subtracted from, or multiplied by. It can be negated by `-x` (only prefix operator in Haskell!) The function `fromInteger` is a conversion function. An integer literal like 42 is the application of `fromInteger` to the appropiate value of type `Integer`

The `Num` class has two subclasses, `Real` and `Fractional` for those respective numbers. `Real` numbers can be ordered, and the only new method is a conversion function from elements in the class to elements of `Rational`. This is essentially a synonym for pairs of integers. The `fractional` numbers are those on which division is defined. Complex numbers cannot be real, but they can be fractional. A floating-point literal like 3.149 is the application of `fromRational` to an appropiate rational number. So `3.149 :: Fractional a => a`.

So when we are adding numbers, with the earlier type `Num a => a` for 42 is why we can form expressions such as `42 + 3.149`. This would produce a type `42 + 3.149 :: Fractional a => a`.

### Haskell Lists
Lists are important because they are the workhorse of functional programming. They can be used to fetch and carry data from one function to another. 
List notation itself, `[1,2,3]`, is an abbreviation for a more basic form `1:2:3:[]`. We can treat them as a singly linked list. I can already see it in how Haskell has worked with it so far (the whole (x:xs) or head tail structure). So functions that work with lists are considered recursive, higher order types (takes in a function as an argument).

The operator `(:) :: a -> [a] -> [a]` (cons) is a constructor for lists. It associates to the right so there isnt a need for parenthesis.

Every list of type a takes either
- Undefined `undefined :: [a]`
- Empty `[] :: [a]`
- A list of the form `x:xs` where `x :: a` and `xs :: [a]`

So there are
- Finite lists, built from `(:)` and `[]`; such as `1:2:3:[]`
- Partial lists, built from `(:)` and `undefined`; such as the list `filter (<4) [1..]` which is the partial list `1:2:3:undefined`.
- Infinite lists, built from `(:)` alone; such as `1[..]`

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
- `map f xs = [f x | x <- xs]`
- `filter p xs = [x | x <- xs, p x]`
- `concat xss [x | xs <- xss, x <- xs]`
However in Haskell, this works the other way around. List comprehensions are translated into equivalent definitions in terms of map and concat. The translation rules are 
- `[e |True] = [e]`
- `[e | q] = [e | q, True]`
- `[e | b, Q] = if b then [e | Q] else []`
- `[e | p <- xs, Q] = let ok p = [e | Q]`
- `                       ok _ = []     `
- `                      in concat (map ok xs)`
Here the definition of `ok` in the fourth rules uses a _don't care_ pattern, also a _wild card_. The `p` in the fourth rule is a pattern, and the definition of `ok` says that the empty list is returned on any argument that doesn't match the pattern `p`.
Another useful rule is
- `[e | Q1, Q2] = concat [[e | Q2] | Q1]`
### Basic operations
Functions can be defined over lists by pattern matching.
```
null :: [a] -> Bool
null [] = True
null (x:xs) = False
```
Alternatively, with a _don't care_ pattern.
```
null [] = True
null _ = False
```

`[]` and `(x:xs)` are patterns that are disjoint and exhaustive, so we can write the two equations for null in any order.

Example of map toUpper for a list of words (since map toUpper only works on one 'word')
```
words = ["word1", "word2"]
allToUpper (x:xs) = map toUpper x : allToUpper xs
allToUpper [] = []
```
Also `map (map toUpper) words`
### Concatenation
The definition for `(++)`, the concatenation operation
```
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)
```


### concat, map, and filter
These are all very useful list operations. Here are their definitions using pattern matching:
```
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x:map f xs

filter p = concat . map (test p)
test p x = if p x then [x] else []
```

### zip and zipWith
`zip` and `zipWith` are functions that either take two lists and combines them into a list of pairs and stops when the shortest is exhausted, or applies a function to pairs of elements from two lists, produving a new list. They are defined in the standard prelude as
```
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y): zip xs ys
zip = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f = []
```
Alternatively, we can write this with a 'don't care' pattern
```
zip xs [] = []
zip [] ys = []
zip (x:xs) [] = []
zip (x:xs) (y:ys) = (x,y) :zip xs ys
```
We could also define zip as `zip = zipWith (,)` where `(,)` is a constructor for pairs: `(,) a b = (a,b)`.

If we want to determine whether a list is in nondecreasing order, we can use `zipWith`.
```
nondec :: (Ord a) => [a] -> Bool
nondec [] = True
nondec [x] = True
nondec (x:y:xs) = (x <= y) && nondec (y:xs)
```
Or a shorter, non-direct, definition
```
nondec xs = and (zipWith (<=) xs (tail xs))
```
`and` is also another useful function in the standard prelude. It takes in a list of booleans and returns `True` if all of the elements are true, and `False` otherwise.

An example of zipWith for temperatures and errors
```
zipWith (+) temps errs
zipWith (-) temps errs
zipWith (\t e -> (t-e, t+e) temps errs)
```
### Folding
*Folding* is the process of recursively applying a function to the elements of a list to accumulate a single result. (Like literally folding a list onto itself)
```
[a] -> b
(a -> a -> a) -> [a] -> a
 0 [1,2,3,4,5]
    1
    (1+2)3
        (3+3)6
            (6+4)10
                (10+5) = 15
```
or another way of looking at it (discrete maths much?)
```
      Left Fold                  Right Fold
0 + 1:[2,3,4,5]            | 1:[2,3,4,5]
(0+1) + 2:[3,4,5]          | 1 + 2:[3,4,5]
((0+1)+2) + 3:[4,5]        | 1 + (2+3:[4,5])
(((0+1)+2)+3) + 4:[5]      | 1 + (2+(3+4:[5]))
((((0+1)+2)+3)+4) + 5:[]   | 1 + (2+(3+(4+5:[])))
                           | 1 + (2+(3+(4+(5+0))))
          15               |          15
```
Now writing this in code.
```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr g z xs
    | null xs = z
    | otherwise = g (head xs) (foldr g z (tail xs))

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl h z xs
    | null xs = z
    | otherwise = foldl h (h z (head xs)) (tail xs)
```
Making a factorial function using fold
```
factorial n = foldl (*) 1 [1..n]
(if communicative, then we change foldl to foldr)
```
## Chapter 6 (Proofs)

## Practice 2 Problems
The pratice 2 problems weren't that bad. The in class ones, especially the applyToEach helped me understand better how Haskell handles lists. However, the Kattis problems proved to be a bit more difficult.



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

## Chapter 5 (Sudoku)
### Specification
We can start by creating types for each unit, such as matrix and row.
- `type Matrix a = [Row a]`
- `type Row a = [a]`
Since a matrix is a list of rows, where a m * n matrix is a list of m rows and in each row is a list with the same length n. Haskell type synonyms can't enforce these constraints. So further definition is required.
A grid is a 9 x 9 matrix of digits
- `type Grid = Matrix Digit`
- `type Digit = Char`
Valid digits are 1 - 9, with 0 being a blank.
- `digits :: [Char]`
- `digits = ['1' .. '9']` (We can do this since Char is an instance of Enum)
- `blank :: Digit -> Bool`
- `blank = (== '0')`

There are many approaches to solving a sudoku puzzle. One way is that we can start with the given grid, and complete it by filling in every possible choice for the blank entires. This results in a list of filled grids. After, we can filter this list for those that don't contain duplicates in any row, box, or column. This is implemented using
```
solve :: Grid -> [Grid]
solve = filter valid . completions
```
where the subsidiary (helper) functions have types
```
completions :: Grid -> [Grid]
valid :: Grid -> Bool
```
We can work on completions by defining it as a two step process. Choices is expanded by creating a func choice that installs the avaliable digits for each cell. If the cell is blank, then all digits are installed
```
completions = expand . choices
choices :: Grid -> Matrix [Digit]
choices = map (map choice)
    where 
        choice d = if blank d then digits else [d]
expand :: Matrix [Digit] -> [Grid]
```
In expand, we want to use the cartesian product to expand out all of the possible row values. Essentially, we are breaking up its argument list into two possibilites, the empty list `[]` and a non empty list `xs:xss` (Remember that xs is just a list, and xss list of lists). We can define it as cp. So it breaks down further to
```
expand :: Matrix Choices -> [Grid]
expand = cp . map cp
```
Suppose that we assume `cp [[2], [1,3]]` results in `[[2,1], [2,3]]`. If we wanted to extend this definition to `cp ([1,2,3] : [[2], [1,3]])`, then we need to prefix 1 to every element of `cp [[2], [1,3]]`, then to prefix 2 to every element of the same list, and finally to prefix 3 to every element.

```
cp :: [[a]] -> [[a]]                List of 4 lists of 3 elements -> List of 3^4 lists of 4 elements
cp [] = [[]]
cp (xs:xss) =                       In this list comprehension, it breaks it down into the beginning list (xs), and the remaining lists (xss).
    [x:ys | x <- xs, ys <- cp xss]  This constructs new lists by taking each x from xs, recursively computing cp xss to get all combinations (ys) from the remaining lists, 
                                    then prepending x to each combination ys to for a new list x:ys
```
To complete the valid function, we create a nodups function to find out if a grid has duplicates. Alongside rows, cols, and boxs with a group and ungroup helper
```
nodups :: [Digit] -> Bool
nodups xs = length (nub xs) == length xs        -- Nub removes duplicates from a list
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
```
The rest of valid,
```
valid :: Grid -> Bool
valid g =   all nodups (boxs g) &&
            all nodups (cols g) &&
            all nodups (boxs g)
```

For claritys sake, breaking cp down into its recursive calls for `cp [[1,2], [3,4], [5,6]]` to better understand how the list comprehension works.
- First call
```
    cp ([1,2] : [[3,4], [5,6]])
    = [ x:ys | x <- [1,2], ys <- cp [[3,4], [5,6]] ]
```
- Second call
```
    cp ([3,4] : [[5,6]])
    = [ x:ys | x <- [3,4], ys <- cp [[5,6]] ]
```
- Third call
```
    cp ([5,6] : [])
    = [ x:ys | x <- [5,6], ys <- cp [] ]
```
- Base case
```
    cp []
    = [[]]
```
- Third call resumes
```
    cp [[5,6]]
    = [ x:ys | x <- [5,6], ys <- [[]] ]
    = [ [5], [6] ]

```
- Second call resumes    
```
    cp [[3,4], [5,6]]
    = [ x:ys | x <- [3,4], ys <- [[5], [6]] ]
    = [ [3,5], [3,6], [4,5], [4,6] ]
```
- First call resumes
```
    cp [[1,2], [3,4], [5,6]]
    = [ x:ys | x <- [1,2], ys <- [ [3,5], [3,6], [4,5], [4,6] ] ]
    = [
        [1,3,5], [1,3,6], [1,4,5], [1,4,6],
        [2,3,5], [2,3,6], [2,4,5], [2,4,6]
      ]
```
                    

## Chapter 5 (Sudoku)
### Specification
We can start by creating types for each unit, such as matrix and row.
- `type Matrix a = [Row a]`
- `type Row a = [a]`
Since a matrix is a list of rows, where a m * n matrix is a list of m rows and in each row is a list with the same length n. Haskell type synonyms can't enforce these constraints. So further definition is required.
A grid is a 9 x 9 matrix of digits
- `type Grid = Matrix Digit`
- `type Digit = Char`
Valid digits are 1 - 9, with 0 being a blank.
- `digits :: [Char]`
- `digits = ['1' .. '9']` (We can do this since Char is an instance of Enum)
- `blank :: Digit -> Bool`
- `blank = (== '0')`

There are many approaches to solving a sudoku puzzle. One way is that we can start with the given grid, and complete it by filling in every possible choice for the blank entires. This results in a list of filled grids. After, we can filter this list for those that don't contain duplicates in any row, box, or column. This is implemented using
```
solve :: Grid -> [Grid]
solve = filter valid . completions
```
where the subsidiary (helper) functions have types
```
completions :: Grid -> [Grid]
valid :: Grid -> Bool
```
We can work on completions by defining it as a two step process. Choices is expanded by creating a func choice that installs the avaliable digits for each cell. If the cell is blank, then all digits are installed
```
completions = expand . choices
choices :: Grid -> Matrix [Digit]
choices = map (map choice)
    where 
        choice d = if blank d then digits else [d]
expand :: Matrix [Digit] -> [Grid]
```
In expand, we want to use the cartesian product to expand out all of the possible row values. Essentially, we are breaking up its argument list into two possibilites, the empty list `[]` and a non empty list `xs:xss` (Remember that xs is just a list, and xss list of lists). We can define it as cp. So it breaks down further to
```
expand :: Matrix Choices -> [Grid]
expand = cp . map cp
```
Suppose that we assume `cp [[2], [1,3]]` results in `[[2,1], [2,3]]`. If we wanted to extend this definition to `cp ([1,2,3] : [[2], [1,3]])`, then we need to prefix 1 to every element of `cp [[2], [1,3]]`, then to prefix 2 to every element of the same list, and finally to prefix 3 to every element.

```
cp :: [[a]] -> [[a]]                List of 4 lists of 3 elements -> List of 3^4 lists of 4 elements
cp [] = [[]]
cp (xs:xss) =                       In this list comprehension, it breaks it down into the beginning list (xs), and the remaining lists (xss).
    [x:ys | x <- xs, ys <- cp xss]  This constructs new lists by taking each x from xs, recursively computing cp xss to get all combinations (ys) from the remaining lists, 
                                    then prepending x to each combination ys to for a new list x:ys
```
To complete the valid function, we create a nodups function to find out if a grid has duplicates. Alongside rows, cols, and boxs with a group and ungroup helper
```
nodups :: [Digit] -> Bool
nodups xs = length (nub xs) == length xs        -- Nub removes duplicates from a list
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
```
The rest of valid,
```
valid :: Grid -> Bool
valid g =   all nodups (boxs g) &&
            all nodups (cols g) &&
            all nodups (boxs g)
```

For claritys sake, breaking cp down into its recursive calls for `cp [[1,2], [3,4], [5,6]]` to better understand how the list comprehension works.
- First call
```
    cp ([1,2] : [[3,4], [5,6]])
    = [ x:ys | x <- [1,2], ys <- cp [[3,4], [5,6]] ]
```
- Second call
```
    cp ([3,4] : [[5,6]])
    = [ x:ys | x <- [3,4], ys <- cp [[5,6]] ]
```
- Third call
```
    cp ([5,6] : [])
    = [ x:ys | x <- [5,6], ys <- cp [] ]
```
- Base case
```
    cp []
    = [[]]
```
- Third call resumes
```
    cp [[5,6]]
    = [ x:ys | x <- [5,6], ys <- [[]] ]
    = [ [5], [6] ]

```
- Second call resumes    
```
    cp [[3,4], [5,6]]
    = [ x:ys | x <- [3,4], ys <- [[5], [6]] ]
    = [ [3,5], [3,6], [4,5], [4,6] ]
```
- First call resumes
```
    cp [[1,2], [3,4], [5,6]]
    = [ x:ys | x <- [1,2], ys <- [ [3,5], [3,6], [4,5], [4,6] ] ]
    = [
        [1,3,5], [1,3,6], [1,4,5], [1,4,6],
        [2,3,5], [2,3,6], [2,4,5], [2,4,6]
      ]
```
                    