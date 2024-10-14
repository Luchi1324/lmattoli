
{- 
Proof of sum function
Definitions:
    sum [] = 0
    sum (x:xs) = x + sum xs
Prove that:
    sum (xs ++ ys) = sum xs + sum ys
-}

{- 
Base step (P([]))
Prove that sum (xs ++ ys) = sum xs + sum ys where sum [] = 0
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
-}


{- 
Inductive step
Proving that P(xs) -> P(x:xs) (Our I.H holds true for the next step)
Prove that sum (xs ++ ys) = sum xs + sum ys where sum (x:xs) = x + sum xs
P(xs)

P(x:xs)
sum ((x:xs) ++ ys) = sum (x:xs) + sum ys

       LHS                                     RHS
    (x:xs) ++ ys                          |  sum (x:xs) + sum ys
    (++).2 [concat definiton part 2]      |  sum.2 [sum definition part 2]
    x : (xs ++ ys)                        |  ""
    ""                                    |  ""
    sum ((x:xs) ++ ys)                    |  x + sum xs + sum ys
    sum (x : (xs ++ ys))                  |  
    x + sum xs + sum ys                   = x + sum xs + sum ys

Both sides


    

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
map (f . g) (x:xs)             |  (map f . map g) (x:xs)
={map.2}                       |  ={composition}
(f . g) x : (map (f.g) xs)     |  map f (map g (x:xs))
={composition} for head        |  ={map.2}
f (g x) : (map (f.g) xs)       |  map f (g x : map g xs)
={I.H for (map f . map g) xs}  |  ={map.2}
f (g x) : ((map f . map g) xs) |  f (g x) : map (map g xs)
                               |  ={composition}
                               |  f (g x) : ((map f . map g) xs)
-}
