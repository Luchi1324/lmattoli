
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

