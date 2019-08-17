-- Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.

slice [] _ _ = []
slice xs first last = take (last-first+1) $ drop (first-1) xs

main = do
    print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
