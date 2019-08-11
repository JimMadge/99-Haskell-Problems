-- Find out whether a list is a palindrome.

isPalindrome x = x == reverse x

main = do
    print $ isPalindrome [1,2,3]
    print $ isPalindrome "madamimadam"
    print $ isPalindrome [1,2,4,8,16,8,4,2,1]
