-- tests/tests.hs
module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)


-- Find out why this property fails.
-- for a function
square :: Double -> Double
square x = x * x
-- why does this property not hold? Examine the type of sqrt.
-- Hint: Read about floating point arithmetic and precision if you’re
-- unfamiliar with it.

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

prop_squareAndBackAgain :: Double -> Bool
prop_squareAndBackAgain x = x == squareIdentity x

--      property $ \x -> x + 1 > (x :: Int)

  
runQc :: IO ()
runQc = quickCheck prop_squareAndBackAgain

main :: IO ()
main = quickCheck prop_thereAndBackAgain
