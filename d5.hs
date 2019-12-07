import System.IO  
import Control.Monad
import Data.List.Split
import Data.Array

main = do  
    contents <- readFile "d5_input"
    let input = map readInt . splitOn "," $ contents
    let (_, output, _) = iter 0 ([5], [], listArray (0, length input) input)
    print $ output

readInt :: String -> Int
readInt = read

-- Simulates IO using two int lists
iter :: Int -> ([Int], [Int], Array Int Int) -> ([Int], [Int], Array Int Int)
iter i (input, output, arr)
  = case op of 99 -> (input, output, arr)
               1  -> iter (i + 4) (input, output, arrOp (+)) 
               2  -> iter (i + 4) (input, output, arrOp (*))
               3  -> iter (i + 2) (tail input, output, arrInput)
               4  -> iter (i + 2) (input, (switch m1 1) : output, arr)
               5  -> iter (jump (/= 0)) (input, output, arr)
               6  -> iter (jump (== 0)) (input, output, arr)
               7  -> iter (i + 4) (input, output, arrComp (<))
               8  -> iter (i + 4) (input, output, arrComp (==))
  where
    (op, m1, m2)      = interpretInstruction (arr ! i)
    switch mode index = if mode == 0 then arr ! (arr ! (i + index)) else arr ! 
                        (i + index)
    arrOp f           = arr // [(arr ! (i + 3), f (switch m1 1) (switch m2 2))]
    arrComp f         = arr // [(arr ! (i + 3), if f (switch m1 1) (switch m2 2)
                        then 1 else 0)]
    arrInput          = arr // [(arr ! (i + 1), head input)]
    jump pred         = if pred (switch m1 1) then (switch m2 2) else i + 3

-- Decodes instruction into (opcode, mode 1, mode 2)    
interpretInstruction :: Int -> (Int, Int, Int)
interpretInstruction x
  = (x `rem` 100, x `div` 100 `rem` 10, x `div` 1000 `rem` 10) 
