import System.IO  
import Control.Monad

main = do  
    contents <- readFile "d1_input"
    print . sum $ map (fuel' . readInt) . words $ contents

readInt :: String -> Int
readInt = read

fuel :: Int -> Int
fuel mass = (mass `div` 3) - 2

fuel' :: Int -> Int
fuel' mass
  | newFuel <= 0 = 0
  | otherwise    = newFuel + fuel' newFuel
  where
    newFuel = fuel mass