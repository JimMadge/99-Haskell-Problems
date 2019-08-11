-- Find the K'th element of a list. The first element in the list is number 1.

-- Using build in function (cheating)
-- elementAt list elem = list !! (elem-1)

-- Recursively, not the most readable
elementAt (x:xs) elem
    | elem == 1 = x
    | otherwise = elementAt xs (elem-1)

main = do
    print $ elementAt [1,2,3] 2
    print $ elementAt "haskell" 5
