import System.IO  
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

main = do  
    content <- readFile "d6_input"
    let mappedInput = parseInput $ lines content
    print . (-2 +) . fromJust $ shortestPath "SAN" S.empty mappedInput "YOU"

-- Stores the input into a Map of planets to their orbiting planets    
parseInput :: [String] -> M.Map String [String]
parseInput [] = M.empty
parseInput (line:lines) 
  = M.insertWith (++) orbiter [orbitee] res
  where
    res                 = parseInput lines
    (orbiter, orbitee') = break (== ')') line
    orbitee             = tail orbitee'

-- Breadth first traversal. Recursively sums the number of orbiting planets at -- each step     
getOrbits :: M.Map String [String] -> String -> Int
getOrbits m orbitee
  | M.member orbitee m = length orbiters + sum (map (getOrbits m) orbiters)
  | otherwise          = 0
  where
    orbiters = m M.! orbitee

-- Breadth first traversal considering both inward and outward paths. Visited -- nodes are tracked using a Set    
shortestPath :: String -> S.Set String -> M.Map String [String] -> String -> Maybe Int
shortestPath dst seen graph src 
  | src == dst            = Just 0
  | S.member src seen     = Nothing
  | M.notMember src graph = maybeMininum orbiterPaths
  | otherwise             = maybeMininum allPaths
  where
    orbitees        = graph M.! src
    orbiters        = M.keys $ M.filter (elem src) graph
    seen'           = S.insert src seen
    getPaths xs     = catMaybes $ map (shortestPath dst seen' graph) xs
    orbiterPaths    = getPaths orbiters
    orbiteePaths    = getPaths orbitees
    allPaths        = orbiterPaths ++ orbiteePaths
    maybeMininum xs = if null xs then Nothing else Just (minimum xs + 1)
