-- Eliminate consecutive duplicates of list elements.

-- Remove all leading elements equal to a from a list
removeAllLeading a [] = []
removeAllLeading a (x:xs)
    | a == x = removeAllLeading a xs
    | otherwise = x:(removeAllLeading x xs)

compress [] = []
compress (x:xs) = x:(removeAllLeading x xs)

main = do
    print $ compress "aaaabccaadeeee"
