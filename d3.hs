import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = do
         content <- readFile "d3_input"
         let xs = map (processWires . splitOn ",") (lines content)
         (print . head . tail . sort) $ findSteps (snd $ xs !! 0) (snd $ xs !! 1) (findDups (fst $ xs !! 0) (fst $ xs !! 1))

--- Wires are tracked in a list of triples containing the x y coordinates and 
-- the number of steps it took to get there        
processWires' :: (Int, Int, Int) -> [String] -> [(Int, Int, Int)]
processWires' _ [] = []
processWires' t@(x,y,s) ((cmd:moves):cmds)
    = case cmd of 'R' -> iter (\(x, y, s) -> (x + 1, y, s + 1))
                  'L' -> iter (\(x, y, s) -> (x - 1, y, s + 1))
                  'U' -> iter (\(x, y, s) -> (x, y + 1, s + 1))
                  'D' -> iter (\(x, y, s) -> (x, y - 1, s + 1))
    where
        cnt = readInt moves
        iter f 
            = take cnt results ++ processWires' (results !! cnt) cmds
            where
                results = iterate f t

processWires :: [String] -> ([(Int, Int)], [(Int, Int, Int)])
processWires str = (map fstAndSnd res, res)
  where
    res = processWires' (0, 0, 0) str
    fstAndSnd (x, y, z) = (x, y)
                
findDups :: Ord a => [a] -> [a] -> [a]
findDups xs ys = S.toList ((S.fromList xs) `S.intersection` (S.fromList ys))

leastManhattan :: [(Int, Int)] -> Int
leastManhattan xs = (head . tail . sort) $ map (\(x, y) -> abs x + abs y) xs

-- Iterates through the list of duplicates (intersections), looks up their step 
-- count in the original lists, then adds them. Returns the steps required to -- reach each intersection
findSteps :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [Int]
findSteps _ _ [] = []
findSteps xs ys (t:ts) = (find t xs + find t ys) : findSteps xs ys ts
    where
        find t@(a,b) ((a', b', s):zs)
          | a == a' && b == b' = s
          | otherwise          = find t zs
        
readInt :: String -> Int
readInt = read
