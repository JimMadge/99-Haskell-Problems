-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version
import Data.List

data Item a = Single a | Multiple Int a
    deriving Show

decode :: [Item a] -> [a]
decode [] = []
decode (x:xs) = (unpack x) ++ decode xs
    where unpack (Single a) = [a]
          unpack (Multiple x a) = replicate x a

--- Or, using concatMap as I learned from the online solution

-- unpack (Single a) = [a]
-- unpack (Multiple x a) = replicate x a
-- decode xs = concatMap (unpack) xs

main = do
    print $ decode [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
