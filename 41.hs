-- Given a range of integers by its lower and upper limit, print a list of all
-- even numbers and their Goldbach composition.

-- In most cases, if an even number is written as the sum of two prime numbers,
-- one of them is very small. Very rarely, the primes are both bigger than say
-- 50. Try to find out how many such cases there are in the range 2..3000.

isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

goldbach n = goldbach' n primes
    where primes = [x | x <- [1..], isPrime x]
          goldbach' n (x:xs)
              | isPrime (n-x) = (x,n-x)
              | otherwise = goldbach' n xs

goldbachList low high = map goldbach (filter even [low..high])

goldbachList' low high limit = filter (test limit) $ goldbachList low high
    where test limit (a,b) = (a>limit) && (b>limit)

main = do
    print $ goldbachList 9 20
    print $ goldbachList' 4 2000 50
