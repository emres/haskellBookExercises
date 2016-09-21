-- Some of the exercises from Haskell Book, Chapter 17. Applicatives
import Control.Applicative
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
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Let's do some Applicative exercises!"
