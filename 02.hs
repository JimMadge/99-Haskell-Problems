-- Find the last but one element of a list

myButLast [] = error "Empty list"
myButLast [x] = error "Single element list"
myButLast list = reverse list !! 1

main = do
    print $ myButLast [1,2,3,4]
    print $ myButLast ['a'..'z']
