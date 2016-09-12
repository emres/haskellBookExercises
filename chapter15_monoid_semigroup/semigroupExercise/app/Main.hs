-- Given a datatype, implement the Semigroup instance. Add Semi-group constraints
-- to type variables where needed. Use the Semigroup class from the semigroups
-- library or write your own. When we use <>, we mean the infix mappend from the
-- Semigroup typeclass.
--
-- Note We’re not always going to derive every instance you may want or need in
-- the datatypes we provide for exercises. We expect you to know what you need
-- and to take care of it yourself by this point.

-- 1. Validate all of your instances with QuickCheck. Since Semigroup’s
-- only law is associativity, that’s the only property you need to reuse.

module Main where

import Lib

import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
 arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a

checkTrivial :: IO ()
checkTrivial = quickCheck (semigroupAssoc :: TrivialAssoc)

--------------------------------------------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True  <> BoolConj False = BoolConj False
  BoolConj True  <> BoolConj True  = BoolConj True
  BoolConj False <> BoolConj True  = BoolConj False
  BoolConj False <> BoolConj False  = BoolConj False

boolConjGen :: Gen BoolConj
boolConjGen = do
  a <- arbitrary
  return (BoolConj a)
  
instance Arbitrary BoolConj where
  arbitrary = boolConjGen

type TypeBool = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = quickCheck (semigroupAssoc :: TypeBool)

--------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
  (Three x y z) <> (Three xx yy zz) = Three x y z

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>  Arbitrary (Three a b c) where
  arbitrary = threeGen

type TypeThree  = Three Int Int Int -> Three Int Int Int -> Three Int Int Int -> Bool

checkThree :: IO ()
checkThree = quickCheck (semigroupAssoc :: TypeThree)

--------------------------------------------------------------------------------
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  x <- arbitrary
  y <- arbitrary
  oneof [return $ Fst x,
         return $ Snd y]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

type TypeOr = Or Int Int -> Or Int Int -> Or Int Int -> Bool

checkOr :: IO ()
checkOr = quickCheck (semigroupAssoc :: TypeOr)

main :: IO ()
main = do
  checkTrivial
  checkBoolConj
  checkOr

