-- Given a datatype, implement the Monoid. Add Monoid constraints to type
-- variables where needed. 

-- Note We’re not always going to derive every instance you may want or need in
-- the datatypes we provide for exercises. We expect you to know what you need
-- and to take care of it yourself by this point.

-- Validate all of your instances with QuickCheck. 

module Main where

import Lib

import Data.Monoid
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

--------------------------------------------------------------------------------
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--------------------------------------------------------------------------------
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Monoid BoolConj where
  mappend (BoolConj True) (BoolConj False) = BoolConj False
  mappend (BoolConj True) (BoolConj True) = BoolConj True
  mappend (BoolConj False) (BoolConj True) = BoolConj False
  mappend (BoolConj False) (BoolConj False) = BoolConj False
  mempty = BoolConj True

boolConjGen :: Gen BoolConj
boolConjGen = do
  a <- arbitrary
  return (BoolConj a)
  
instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type TypeBool = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = quickCheck (monoidAssoc :: TypeBool)

checkBoolConjLI:: IO ()
checkBoolConjLI = quickCheck (monoidLeftIdentity :: BoolConj -> Bool)

checkBoolConjRI:: IO ()
checkBoolConjRI = quickCheck (monoidLeftIdentity :: BoolConj -> Bool)



--------------------------------------------------------------------------------
main :: IO ()
main = do
  checkBoolConj
  checkBoolConjLI
  checkBoolConjRI

