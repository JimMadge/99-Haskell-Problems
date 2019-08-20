-- Insert an element at a given position into a list.

insertAt item list position = before ++ [item] ++ after
    where (before, after) = splitAt (position-1) list

main = do
    print $ insertAt 'X' "abcd" 2
