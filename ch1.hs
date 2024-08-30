{- 
    Count the occurances of the n most common words in a textt
-}

import Data.Char( toLower )

type Text = String -- type synonym

-- We can use undefined as a placeholder while we construct our overall program, so we can complie
showRun = undefined
sortRuns = undefined
countRuns = undefined
sortWords = undefined

-- Remember that calls work right to left (covert all to lower, get words, sort the words, count the runs of seperate words, sort said runs, take the top n words, )
commonWords :: Int -> Text -> String
commonWords n = concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map toLower