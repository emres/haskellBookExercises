{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- cap
  y <- rev
  return (x, y)
--------------------------------------------------------------------------------
newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f
--------------------------------------------------------------------------------

-- For more detailed information, please see:
-- http://stackoverflow.com/a/40060292/236007
-- http://comonad.com/reader/2012/abstracting-with-applicatives/

instance Functor (Reader r) where
  fmap f (Reader x) = Reader (f . x)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a
  
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra $ r
  --(Reader rab) <*> (Reader ra) = Reader $ rab <*> ra
--------------------------------------------------------------------------------

-- Implement the Reader Monad
-- Don't forget InstanceSigs.
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> undefined




main :: IO ()
main = putStrLn "Let's do some Reader exercises!"
