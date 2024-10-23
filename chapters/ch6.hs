
{- 
Proof of sum function
Definitions:
    sum [] = 0
    sum (x:xs) = x + sum xs
Prove that:
    sum (xs ++ ys) = sum xs + sum ys

Base step (P([]))
Prove that sum (xs ++ ys) = sum xs + sum ys
sum ([] ++ ys) = sum [] + sum ys
       LHS                                       RHS
    [] ++ ys                                | sum [] + sum ys
    (++).1 [concat definition part 1]       | ""
    ys                                      | 0 + sum ys
    sum ([] ++ ys)                          | ""
    sum (ys)                                | ""
    sum ys                                  | sum ys

Both sides
    sum ([] ++ ys) = sum [] + sum ys 
    ==> sum ys = 0 + sum ys 
    ==> sum ys = sum ys
 
Inductive step
Proving that P(xs) -> P(x:xs) (Our I.H holds true for the next step)
Prove that sum (xs ++ ys) = sum xs + sum ys
P(xs)

P(x:xs)
sum ((x:xs) ++ ys) = sum (x:xs) + sum ys

       LHS                                     RHS
    sum ((x:xs) ++ ys)                    |  sum (x:xs) + sum ys
    =(++).2 [concat definiton part 2]     |  =sum.2 [sum definition part 2]
    sum (x : (xs ++ ys))                  |  x + sum xs + sum ys
    =sum.2                                |  ""
    x + sum (xs ++ ys)                    |  ""
    =IH (sum (xs++ys) = sum xs+sum ys)    |  ""
    x + sum xs + sum ys                   |  x + sum xs + sum ys
-}

{-
Example proof with
map (f . g) xs = (map f . map g) xs

What do we need?
Map
1. map f [] = [] -- Empty case
2. map f (x:xs) = f x : map f xs -- Non empty case
Func composition
1. (f . g) x = f (g x)

Setting up base case
map (f . g) []          |  (map f . map g) []
since (f.g) can be 'f'  |  ={composition}
""                      |  map f (map g [])
""                      |  ={map.1}
={map.1}                |  map f ([])
map 'f' []              |  ={map.1}
={map.1}                |  ""
[]                      |  []

Setting up inductive case
map (f . g) (x:xs)               |  (map f . map g) (x:xs)
={map.2}                         |  ={composition}
(f . g) x : (map (f.g) xs)       |  map f (map g (x:xs))
={composition} for head          |  ={map.2}
f (g x) : (map (f.g) xs)         |  map f (g x : map g xs)
={I.H}                           |  ={map.2}
since (map f . map g) xs = above |  ""
f (g x) : ((map f . map g) xs)   |  f (g x) : map (map g xs)
                                 |  ={composition}
                                 |  f (g x) : ((map f . map g) xs)                               
-}

{-
fmap proofs
maybe fmap definitions
1. fmap f Nothing = Nothing
2. fmap f (Just x) = Just (f x)

fmap (f.g) Nothing
= Nothing

(fmap f . fmap g) Nothing
fmap f (fmap g Nothing)
fmap f Nothing
Nothing

fmap (f . g) (Just x)   | (fmap f . fmap g) (Just x)
={Maybe.fmap.2}         | ={composition}
Just ((f . g) x)        | fmap f (fmap g (Just x))
={Composition}          | ={Maybe.fmap.2}
Just (f (g x))          | fmap f (Just (g x))
                        | ={Maybe.fmap.2}
                        | Just (f (g x))
-}

{-
Definitions
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

# Prior results
xs ++ [] = xs --- (++).rightidentity
(xs ++ ys) ++ xs = xs ++ (ys ++ zs) --- (++).associativity

# Proof
P(xs): reverse (xs ++ ys) = reverse ys ++ reverse xs

Base Case P([]):
reverse ([] ++ ys)  |  reverse ys ++ reverse []
= { (++).1 }        |  = { reverse.1 }
reverse ys          |  reverse ys ++ [] (need to use a prior proof)
                    |  = { (++).rightidentity }
                    |  reverse ys


Inductive Case: P(xs) -> P(x:xs)
reverse ((x:xs) ++ ys)          | reverse ys ++ reverse (x:xs)
= { (++).2 }                    | = { reverse.2 }
reverse (x:(xs ++ ys)           | reverse ys ++ (reverse xs ++ [x])
= { reverse.2 }                 | = { (++).associativity }
reverse (xs ++ ys) ++ [x]       | (reverse ys ++ reverse xs) ++ [x]
= { I.H }                       | ""
reverse ys ++ reverse xs ++ [x] | reverse ys ++ reverse xs ++ [x]
-}