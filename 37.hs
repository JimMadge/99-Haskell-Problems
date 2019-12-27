-- Calculate Euler's totient function phi(m) (improved).

-- See problem 34 for the definition of Euler's totient function. If the list
-- of the prime factors of a number m is known in the form of problem 36 then
-- the function phi(m) can be efficiently calculated as follows: Let ((p1 m1)
-- (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities)
-- of a given number m. Then phi(m) can be calculated with the following
-- formula:

-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...

-- Note that a ** b stands for the b'th power of a.

-- Primefactors from problem 35 solutions
primeFactors 1 = []
primeFactors n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ primeFactors $ div n prime

collect [] = []
collect xs = collect' 1 xs where
    collect' n (x:xs)
        | xs == [] = [(x, n)]
        | x == head xs = collect' (n+1) xs
        | otherwise = (x, n): collect' 1 xs

primeFactorsMult = collect . primeFactors

-- First implementatation
-- phi' [] = 1
-- phi' (x:xs) = ((p-1) * p^(m-1)) * phi' xs
--     where p = fst x
--           m = snd x
--
-- phi = phi' . primeFactorsMult

-- A more 'Haskell' like solution with function composition
f (p,m) = (p-1) * p^(m-1)
phi = product . map f . primeFactorsMult

main = do
    print $ phi 315
    print $ phi 10
