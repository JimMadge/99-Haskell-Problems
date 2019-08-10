-- Find the last element of a list

mylast [] = error "Empty list"
mylast (x:xs)
    | xs == [] = x
    | otherwise = mylast xs

main = do
    print $ mylast [1,2,3,4]
    print $ mylast ['x','y','z']
