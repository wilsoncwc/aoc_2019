import System.IO  
import Control.Monad
import Data.List.Split
import Data.Array

main = do  
    contents <- readFile "d2_input"
    let input = map readInt . splitOn "," $ contents in
        print $ try (listArray (0, length input - 1) input) [(x, y) | x <- [0..100], y <- [0..100]] 19690720

readInt :: String -> Int
readInt = read

iter :: Int -> Array Int Int -> Array Int Int
iter i arr
  = case (arr ! i) of 99 -> arr
                      1  -> iter (i + 4) (arrOp (+)) 
                      2  -> iter (i + 4) (arrOp (*))
  where
    arrOp f = arr // [(arr ! (i + 3), f (arr ! (arr ! (i + 1))) (arr ! (arr ! (i + 2))))]

-- Iterates through all the pairs until it finds the correct result    
try :: Array Int Int -> [(Int, Int)] -> Int -> Int
try arr ((n,v):ps) ans
  | (iter 0 (arr // [(1, n), (2, v)])) ! 0 == ans = 100 * n + v
  | otherwise                                     = try arr ps ans  

