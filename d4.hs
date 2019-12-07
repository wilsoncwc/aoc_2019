import Data.Digits

validPassword :: Int -> Bool
validPassword n
  = verifyDouble ds && verifyNotDecreasing ds
  where
    ds              = digits 10 n
    verifyDouble [] = False
    verifyDouble [x] = False
    verifyDouble xs'@(x:xs)
      | x == head xs = if (length eq == 2) then True else verifyDouble neq
      | otherwise    = verifyDouble xs
      where
        (eq, neq) = span (x ==) xs'
    verifyNotDecreasing [_] = True
    verifyNotDecreasing (x:xs)
      | head xs < x = False
      | otherwise   = verifyNotDecreasing xs  
      
countValid :: Int -> Int -> (Int -> Bool) -> Int
countValid start end f = length $ filter f [start..end] 