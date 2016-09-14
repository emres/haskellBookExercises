module Main where

import Lib

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

main :: IO ()
main = 
  putStrLn "Let's solve some Functor exercises!"
