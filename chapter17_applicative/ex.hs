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
maxed = max' <$> x2  <*> y2

main :: IO ()
main = do
  putStrLn "Let's do some Applicative exercises!"
