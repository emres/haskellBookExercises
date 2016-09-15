module Main where

import Lib
import Test.QuickCheck

--------------------------------------------------------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


--------------------------------------------------------------------------------
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

identityGen :: (Arbitrary a) => Gen (Identity a)
identityGen = do
  x <- arbitrary
  return (Identity x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = identityGen

checkIdentityIdentity :: IO ()
checkIdentityIdentity = quickCheck $ \x -> functorIdentity (x :: (Identity Int))
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Let's solve some Functor exercises!"
  checkIdentityIdentity
