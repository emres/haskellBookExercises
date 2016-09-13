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
import Control.Arrow ((&&&))

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
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

-- instance (Monoid a) => Monoid (Mem s a) where
--   mempty      = Mem (\s -> (mempty, s))
--   mappend (Mem f) (Mem g) = Mem (f . snd . g)

-- instance (Monoid a) => Monoid (Mem s a) where
--   mempty      = Mem (\s -> (mempty, s))
--   mappend (Mem f) (Mem g) =
--     Mem $ \x -> ( (fst . f <> fst . g) x
--                 , (snd . f . snd . g)  x )

-- Usage of Control.Arrow.&&& was suggested by ghc-mod
instance (Monoid a) => Monoid (Mem s a) where
  mempty      = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) = Mem $ (fst . f <> fst . g) &&& (snd . f . snd . g)



-- Given the following code:
f' = Mem $ \s -> ("hi", s + 1)

-- main = do
--   print $ runMem (f' <> mempty) 0
--   print $ runMem (mempty <> f') 0
--   print $ (runMem mempty 0 :: (String, Int))
--   print $ runMem (f' <> mempty) 0 == runMem f' 0
--   print $ runMem (mempty <> f') 0 == runMem f' 0

-- A correct Monoid for Mem should, given the above code, get the following
-- output:

--     Prelude> main
--      ("hi",1)
--      ("hi",1)
--      ("",0)
--      True
--      True

--------------------------------------------------------------------------------
main :: IO ()
main = do
  checkBoolConj
  checkBoolConjLI
  checkBoolConjRI
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0

