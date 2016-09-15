{-# LANGUAGE ViewPatterns #-}

module Main where

import Lib
import Test.QuickCheck
import Test.QuickCheck.Function

--------------------------------------------------------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
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
checkIdentityIdentity = quickCheck $ \x -> functorIdentity (x :: Identity Int)


li x = functorCompose (+1) (*2) (x :: Identity Int)
checkIdentityCompose :: IO ()
checkIdentityCompose = quickCheck li

type IntToInt = Fun Int Int
type IntFC = (Identity Int) -> IntToInt -> IntToInt -> Bool
checkIdentityFunctional :: IO ()
checkIdentityFunctional = quickCheck (functorCompose' :: IntFC)
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Let's solve some Functor exercises!"
  checkIdentityIdentity
  checkIdentityCompose
  checkIdentityFunctional
