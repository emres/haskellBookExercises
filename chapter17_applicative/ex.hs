-- Some of the exercises from Haskell Book, Chapter 17. Applicatives
--import Control.Applicative
--import Data.Monoid
import Data.List (elemIndex)

-- f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
-- g x = lookup x [(7, "sup?"), (8, "chris"), (9, "aloha")]
-- h x = lookup x [(2, 3), (5, 6), (7, 8)]
-- m x = lookup x [(4, 10), (8, 13), (1, 9001)]

--------------------------------------------------------------------------------
-- make the following expressions type check
----------
added :: Maybe Integer
added = (+3) <$> (lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup (2 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

----------
x2 :: Maybe Int
x2 = elemIndex (3 :: Int) [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex (4 :: Int) [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x2 <*> y2

----------
xs = [1, 2, 3]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (fmap . fmap) sum (,) <$> x3 <*> y3

--------------------------------------------------------------------------------
-- Write an Applicative instance for Identity.
-- so that:
-- Prelude> const <$> [1, 2, 3] <*> [9, 9, 9]
-- [1,1,1,2,2,2,3,3,3]
-- Prelude> const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
-- Identity [1,2,3]
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  --pure :: a -> Identity a
  pure x = Identity x
  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) (Identity f) (Identity x) = Identity (f x)

--------------------------------------------------------------------------------
-- Write an Applicative instance for Constant.
--
-- > Constant (Sum 1) <*> Constant (Sum 2)
-- Constant {getConstant = Sum {getSum = 3}
--
-- > Constant undefined <*> Constant (Sum 2)
-- Constant (Sum {getSum = *** Exception: Prelude.undefined
--
-- > pure 1 :: Constant String Int
-- Constant {getConstant = ""}

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  -- pure :: a -> f a
  -- pure :: a -> Constant e a
  pure _ = Constant mempty

  -- f ~ Constant e
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b
  (<*>) (Constant x) (Constant y) = Constant (mappend x y)


--------------------------------------------------------------------------------
-- Exercise: Fixer Upper
-- Given the function and values provided, use (<$>) from
-- Functor, (<*>) and pure from the Applicative typeclass to
-- fill in missing bits of the broken code to make it work.

-- Solutions:
-- const <$> Just "Hello" <*> pure "World"
-- (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

--------------------------------------------------------------------------------
-- Write the Applicative instance for `List a`
-- Expected result:
-- Prelude> let functions = Cons (+1) (Cons (*2) Nil)
-- Prelude> let values = Cons 1 (Cons 2 Nil)
-- Prelude> functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x = (Cons x Nil)
  (<*>) _            Nil          = Nil
  (<*>) Nil          _            = Nil
  (<*>) (Cons f Nil) (Cons x Nil) = Cons (f x) Nil
  (<*>) (Cons f Nil) x            = f <$> x
  (<*>) (Cons f fs)  (Cons x Nil) = Cons (f x) (fs <*> (Cons x Nil))
  (<*>) (Cons f fs)  (Cons x xs)  = Cons (f x) (fs <*> xs)
  

--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Let's do some Applicative exercises!"
