{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

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
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = (Pair (f x) (f y))

pairGen :: (Arbitrary a) => Gen (Pair a)
pairGen = do
  x <- arbitrary
  y <- arbitrary
  return (Pair x y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = pairGen
  
checkIdentityPair :: IO ()
checkIdentityPair = quickCheck $ \x -> functorIdentity (x :: Pair Int)

helperPair x = functorCompose (+1) (*2) (x :: Pair Int)
checkPairCompose :: IO ()
checkPairCompose = quickCheck helperPair

type PairIntFC = (Pair Int) -> IntToInt -> IntToInt -> Bool
checkPairFunctional :: IO ()
checkPairFunctional = quickCheck (functorCompose' :: PairIntFC)

--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen
  
checkIdentityTwo :: IO ()
checkIdentityTwo = quickCheck $ \x -> functorIdentity (x :: Two Int String)

helperTwo x = functorCompose (+1) (*2) (x :: Two String Int)
checkTwoCompose :: IO ()
checkTwoCompose = quickCheck helperTwo

type StringToDouble = Fun String Double
type IntToString = Fun Int String
type TwoStringIntFC = (Two String Int) -> IntToString -> StringToDouble-> Bool
checkTwoFunctional :: IO ()
checkTwoFunctional = quickCheck (functorCompose' :: TwoStringIntFC)

--------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen
  
checkIdentityThree :: IO ()
checkIdentityThree = quickCheck $ \x -> functorIdentity (x :: Three Float Int String)

helperThree x = functorCompose (+1) (*2) (x :: Three Int Int Int)
checkThreeCompose :: IO ()
checkThreeCompose = quickCheck helperThree

type ThreeStringIntFC =
  (Three String String Int) -> IntToString -> StringToDouble-> Bool
checkThreeFunctional :: IO ()
checkThreeFunctional = quickCheck (functorCompose' :: ThreeStringIntFC)

--------------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

three'Gen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
three'Gen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three' x y z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
   arbitrary = three'Gen

checkIdentityThree' :: IO ()
checkIdentityThree' =
  quickCheck $ \x -> functorIdentity (x :: Three' Float String)

helperThree' x = functorCompose (+1) (*2) (x :: Three' Int Int)
checkThree'Compose :: IO ()
checkThree'Compose = quickCheck helperThree'

--------------------------------------------------------------------------------
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

--------------------------------------------------------------------------------
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v


--------------------------------------------------------------------------------
-- Rearrange the arguments to the type constructor of the datatype, so the
-- Functor instance works.
data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b


----------
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


----------
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--------------------------------------------------------------------------------
-- Write Functor instances for the following datatypes.

----------
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk x) = Desk x
  fmap f (Bloor y) = Bloor (f y)

----------
-- data K a b =
--   K a

-- instance Functor (K a) where
--   fmap f (K x) = K x

----------
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a

instance Functor (K a) where
  fmap f (K x) = K x

-- should remind you of an
-- instance you've written before
-- For explanation, see the following:
-- http://stackoverflow.com/questions/39558488/how-to-write-the-functor-instance-for-this-type
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))


----------
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

----------
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
 fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

----------
-- Don’t ask for more typeclass instances than you need.
-- You can let GHC tell you what to do.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

----------
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

----------
data List a =
    Nil
  | Cons a (List a)

-- instance Functor List where
--   fmap f Nil = Nil
--   fmap f (Cons x _) = Cons (f x)
  
main :: IO ()
main = do
  putStrLn "Let's solve some Functor exercises!"
  checkIdentityIdentity
  checkIdentityCompose
  checkIdentityFunctional
  
  checkIdentityPair
  checkPairCompose
  checkPairFunctional
  
  checkIdentityTwo
  checkTwoCompose
  checkTwoFunctional
  
  checkIdentityThree
  checkThreeCompose
  checkThreeFunctional

  checkIdentityThree'
