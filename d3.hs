import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = do
         content <- readFile "d3-input"
         let xs = map (processWires (0, 0) . splitOn ",") (lines content)
         print . leastManhattan $ findDups (xs !! 0) (xs !! 1)
         

processWires :: (Int, Int) -> [String] -> [(Int, Int)]
processWires _ [] = []
processWires (x,y) ((cmd:moves):cmds)
    = case cmd of 'R' -> iter (\(x, y) -> (x + 1, y))
                  'L' -> iter (\(x, y) -> (x - 1, y))
                  'U' -> iter (\(x, y) -> (x, y + 1))
                  'D' -> iter (\(x, y) -> (x, y - 1))
    where
        cnt = readInt moves
        iter f 
            = take cnt results ++ processWires (results !! cnt) cmds
            where
                results = iterate f (x, y)
            
findDups :: Ord a => [a] -> [a] -> [a]
findDups xs ys = S.toList ((S.fromList xs) `S.intersection` (S.fromList ys))

leastManhattan :: [(Int, Int)] -> Int
leastManhattan xs = (head . tail . sort) $ map (\(x, y) -> abs x + abs y) xs
        
readInt :: String -> Int
readInt = read
