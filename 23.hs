-- Extract a given number of randomly selected elements from a list.
import System.Random

rnd_select [] _ = []
rnd_select xs n = extract xs selected
    where size = length xs
          selected = take n $ randomRs (0, size-1) (mkStdGen 3454) :: [Int]

extract xs [] = []
extract xs elems = xs !! (head elems) : extract xs (tail elems)

main = do
    print $ rnd_select "abcdefgh" 3
