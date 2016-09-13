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
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- See the following for discussion about this question:
-- http://stackoverflow.com/questions/39456716/how-to-write-semigroup-instance-for-this-data-type

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

-- instance Semigroup (Combine a b) where
--   (Combine f) <> (Combine g) = Combine (f and g appended, whatever that concrete append operation is, so I think this requires <> from Monoid, but this coincides the one from Semigroup, what's a programmer to do?) 

-- instance Semigroup (Combine a b) where
--   Combine f <> Combine g = Combine f

-- This is also a Monoid?
-- instance Monoid b => Semigroup (Combine a b) where
--   (Combine f) <> (Combine g) = Combine (\x -> f x `mappend` g x)

-- Let's remove the Monoid, and only have Semigroup
instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

-- TODO how to write quickCheck for Combine? How to use CoArbitrary?

--------------------------------------------------------------------------------

-- Hint: We can do something that seems a little more specific and natural to
-- functions now that the input and output types are the same.

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f <> g)

-- TODO how to write quickCheck for Comp? How to use CoArbitrary?

--------------------------------------------------------------------------------
data Validation a b =
    Fail a
  | Succ b
  deriving (Eq, Show)

-- Type checks, but probably does not behave as expected!
instance Semigroup a => Semigroup (Validation a b) where
  Fail a <> Succ b = Fail a
  Succ b <> Fail a = Fail a
  Succ a <> Succ b = Succ b
  Fail a <> Fail b = Fail a

--------------------------------------------------------------------------------
main :: IO ()
main = do
  checkTrivial
  checkBoolConj
  checkOr

