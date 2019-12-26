-- Calculate Euler's totient function phi(m).

-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.

-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4.
-- Note the special case: phi(1) = 1.

coprime a b = gcd a b == 1

phi 1 = 1
phi a = length coprimes
    where coprimes = [x | x <- [1..(a-1)], coprime a x]

main = do
    print $ phi 10
