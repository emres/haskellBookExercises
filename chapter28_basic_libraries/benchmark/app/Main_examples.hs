module MainExamples where

import Criterion.Main
import qualified Data.Map as M

infixl 9 !?
_ !? n | n < 0 = Nothing
[] !? _        = Nothing
(x:_) !? 0     = Just x
(_:xs) !? n    = xs !? (n-1)
  
myList :: [Int]
myList = [1..9999]

-- main :: IO ()
-- main = defaultMain
--   [ bench "index list 9999"
--     $ whnf (myList !!) 9998
--   , bench "index list maybe index 9999"
--     $ whnf (myList !?) 9998
--   ]

-- main :: IO ()
-- main = defaultMain
--   [ bench "map list 9999" $ whnf (map (+1)) myList ]

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0 xs = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

main :: IO ()
main = defaultMain
  [ bench "lookup one thing, list" $ whnf (lookup "doesntExist") pairList
  , bench "lookup one thing, map" $
    whnf (M.lookup "doesntExist") testMap
  ]
