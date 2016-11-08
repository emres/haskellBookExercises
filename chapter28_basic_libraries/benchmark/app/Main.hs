module Main where

import Criterion.Main
import qualified Data.Map as M

newtype DList a =
  DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = \x -> DL (x :)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList wrappedFunctionForXs = (unDL wrappedFunctionForXs) []
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr `cons`
cons      :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist. infixl `snoc`
snoc :: DList a -> a -> DList a
snoc wrappedFnForXs x = DL $ (unDL wrappedFnForXs . (x:))
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append wrappedFnForXs wrappedFnForYs = DL $
                                       (unDL wrappedFnForXs) .
                                       (unDL wrappedFnForYs)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]

