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
snoc = undefined
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append = undefined
{-# INLINE append #-}

main :: IO ()
main = do
  putStrLn "some benchmarking for DList"
