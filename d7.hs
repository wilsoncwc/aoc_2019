import System.IO  
import Control.Monad
import Data.List
import Data.List.Split
import Data.Array

main = do  
    contents <- readFile "d7_input"
    let input = map readInt . splitOn "," $ contents
        prog  = listArray (0, length input) input
    print $ tryPhases prog (permutations [5..9])

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

-- Feeds the output of the first machine into the next, and returns the output -- of the last machine
amplify :: Array Int Int -> Int -> [Int] -> Int
amplify _ out [] = out
amplify prog input (phase:phases) 
  = amplify prog (head output) phases
  where
    (_, output, _) = iter 0 ([phase, input], [], prog)

-- Obtains the maximum possible output from all the permutations    
tryPhases :: Array Int Int -> [[Int]] -> Int
tryPhases prog tries = maximum $ map (amplify' prog) tries   

-- Initializes the feedback loop with the phase settings
amplify' :: Array Int Int -> [Int] -> Int
amplify' prog phases
  = head endOutput
  where
    progA@(_, aoutput, _) = iter' 0 ([phases !! 0, 0], [], prog)
    progB@(_, boutput, _) = iter' 0 ([phases !! 1, head aoutput], [], prog)    
    progC@(_, coutput, _) = iter' 0 ([phases !! 2, head boutput], [], prog)    
    progD@(_, doutput, _) = iter' 0 ([phases !! 3, head coutput], [], prog)    
    progE = iter' 0 ([phases !! 4, head doutput], [], prog)    
    endProgs = feedbackLoop [progA, progB, progC, progD, progE]
    (_, endOutput, _) = endProgs !! 4

-- Repeatedly calls iter' on each of the machines until E halts fully (i.e. 
-- returned input list is null)   
feedbackLoop :: [([Int], [Int], Array Int Int)] -> [([Int], [Int], Array Int Int)]
feedbackLoop progs
  | null einput = progs
  | otherwise = feedbackLoop [progA', progB', progC', progD', progE']
  where
    (ainput, aoutput, progA) = progs !! 0
    (binput, boutput, progB) = progs !! 1
    (cinput, coutput, progC) = progs !! 2
    (dinput, doutput, progD) = progs !! 3
    (einput, eoutput, progE) = progs !! 4
    progA'@(_, aoutput', _) = iter' (head ainput) ([head eoutput], aoutput, progA)
    progB'@(_, boutput', _) = iter' (head binput) ([head aoutput'], doutput, progB)
    progC'@(_, coutput', _) = iter' (head cinput) ([head boutput'], coutput, progC)
    progD'@(_, doutput', _) = iter' (head dinput) ([head coutput'], doutput, progD)
    progE' = iter' (head einput) ([head doutput'], eoutput, progE)
    
-- Version of iter that "pauses" on anticipating input. Saves the instruction -- pointer in the input list.
iter' :: Int -> ([Int], [Int], Array Int Int) -> ([Int], [Int], Array Int Int)
iter' i (input, output, arr)
  = case op of 99 -> ([], output, arr)
               1  -> iter' (i + 4) (input, output, arrOp (+)) 
               2  -> iter' (i + 4) (input, output, arrOp (*))
               3  -> if null input then ([i], output, arr)
                     else iter' (i + 2) (tail input, output, arrInput)
               4  -> iter' (i + 2) (input, (switch m1 1) : output, arr)
               5  -> iter' (jump (/= 0)) (input, output, arr)
               6  -> iter' (jump (== 0)) (input, output, arr)
               7  -> iter' (i + 4) (input, output, arrComp (<))
               8  -> iter' (i + 4) (input, output, arrComp (==))
  where
    (op, m1, m2)      = interpretInstruction (arr ! i)
    switch mode index = if mode == 0 then arr ! (arr ! (i + index)) else arr ! 
                        (i + index)
    arrOp f           = arr // [(arr ! (i + 3), f (switch m1 1) (switch m2 2))]
    arrComp f         = arr // [(arr ! (i + 3), if f (switch m1 1) (switch m2 2)
                        then 1 else 0)]
    arrInput          = arr // [(arr ! (i + 1), head input)]
    jump pred         = if pred (switch m1 1) then (switch m2 2) else i + 3    