-- Finding how many words start with a using list functions
words = ["apple", "banana", "dates", "pineapple"]

-- first get all the letters, then filter for 'a', then get lenght
length (filter (=='a') (map head words))

-- func composition
filter ((=='a') . head) words
-- lambda notation (for our filter)
-- after the \ is our parameter list, then the -> is the actual function
filter (\word -> head word == 'a) words

-- Working down to point free
startsWithLowerA word = head word == 'a'
startsWithLowerA word = (==) 'a' (head word)
startsWithLowerA word = ((==) 'a' . head) word
startsWithLowerA = ((==) 'a' . head)

-- Example with taking i 
ys = map (\x -> x^2 + 2 * x - 1) [0..]

ys !! 5
-- produces 34

fmap -- ??