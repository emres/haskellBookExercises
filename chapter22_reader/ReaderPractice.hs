module ReaderPractice where

--import Control.Applicative
import Data.Maybe

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ []       = Nothing
lookup' x' listOfTuples =
  listToMaybe $ catMaybes $ map (helper x') listOfTuples
  where helper x'' (y', z'')
          | x'' == y' = Just z''
          | otherwise = Nothing

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup' 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup' 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key zs :: Maybe Integer
zs :: Maybe Integer
zs = lookup' 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x y

-- Now we want to add the ability to make a Maybe (,) of values
-- using Applicative. Have x1 make a tuple of xs and ys, and x2
-- make a tuple of of ys and zs. Also, write x3 which takes one
-- input and makes a tuple of the results of two applications of z' from above.
--
-- Your outputs from those should look like this:
-- *ReaderPractice> x1
-- Just (6,9)
-- *ReaderPractice> x2
-- Nothing
-- *ReaderPractice> x3 3
-- (Just 9,Just 9)
x1 :: Maybe (Integer, Integer)
x1 = undefined

x2 :: Maybe (Integer, Integer)
x2 = undefined

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = undefined
