module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = error $ "Only accepts a single digit!"

digits :: Int -> [Int]
digits n = go n 0
  where go n count
         | div n (10 ^ count) == 0 = []
         | otherwise = (go n (count + 1)) ++ [mod (div n (10 ^ count)) 10]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

-- mod (div 12345 (10^4)) 10 :
-- mod (div 12345 (10^3)) 10 :
-- mod (div 12345 (10^2)) 10 :
-- mod (div 12345 (10^1)) 10 : 
-- mod (div 12345 (10^0)) 10 :
-- []
