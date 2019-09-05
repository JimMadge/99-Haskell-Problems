-- Lotto: Draw N different random numbers from the set 1..M.
import System.Random
import Data.List

draw :: Eq a => [a] -> Int -> StdGen -> [a]
draw [] _ _ = []
draw _ 0 _ = []
draw xs n gen = selected : draw (delete selected xs) (n-1) newgen
    where
        selected = xs !! selected_elem
        (selected_elem, newgen) =  randomR (0, (length xs)-1) gen :: (Int, StdGen)

diff_select n upper = draw [1..upper] n

main = do
    gen <- getStdGen
    -- print $ draw [1..5] 2 gen
    print $ diff_select 6 49 gen
