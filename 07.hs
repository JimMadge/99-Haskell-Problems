-- Flatten a nested list structure.

-- Nested list data type
data NestedList a = Elem a | List [NestedList a]
    deriving Show

flatten :: (NestedList a) -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

main = do
    print $ flatten (Elem 5)
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    -- print $ flatten (List [])
