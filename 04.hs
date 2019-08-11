-- Find the number of elements in a list

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

main = do
    print $ myLength [123, 456, 789]
    print $ myLength "Hello, world!"
