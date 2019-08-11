-- Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates
-- of elements are encoded as lists (N E) where N is the number of duplicates of
-- the element E.

pack [] = []
pack list@(x:xs) = first : pack rest
    where
    (first, rest) = span (==x) list

runLength [] = []
runLength (x:xs) = (length x, head x) : runLength xs

encode list = runLength $ pack list

-- Consise lambda and list comphension solutions from the wiki
-- encode xs = map (\x -> (length x,head x)) (group xs)
-- encode xs = [(length (x:xs), x) | (x:xs) <- pack xs]

main = do
    print $ encode "aaaabccaadeeee"
