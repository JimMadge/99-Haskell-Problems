-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

-- Import List module for the group function
import Data.List

-- Multiple or single data type
data Item a = Single a | Multiple Int a
    deriving Show

runLengthModified [] = []
runLengthModified (x:xs)
    | length x == 1 = Single x : runLengthModified xs
    | otherwise = Multiple (length x) x : runLengthModified xs

encodeModified xs = runLengthModified $ group xs

main = do
    print $ encodeModified "aaaabccaadeeee"
