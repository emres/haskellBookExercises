import Data.Foldable
import Data.Monoid

-- Exercises: Library Functions
-- Implement the functions in terms of foldMap or foldr from Foldable,
-- then try them out with multiple types that have Foldable instances.

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = any . (==)

--------------------------------------------------------------------------------
-- For details, alternatives, and discussions, see the following:
-- http://stackoverflow.com/q/37398457/236007
-- http://stackoverflow.com/q/12216886/236007
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr maybeMin Nothing
  where
    maybeMin x Nothing = Just x
    maybeMin x (Just y) = Just (min x y)

-- Using type holes
-- mini :: (Foldable t, Ord a) => t a -> Maybe a
-- mini xs = foldr _ Nothing xs

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr max' Nothing
  where
    max' x Nothing = Just x
    max' x (Just y) = Just (max x y)

-- See the following thread
-- https://mail.haskell.org/pipermail/haskell-cafe/2007-September/032014.html
-- as well as:
-- http://stackoverflow.com/q/11425334/236007
length :: (Foldable t) => t a -> Int
length = foldr (\_ n -> 1 + n)  0

-- Some say this is all Foldable amounts to.
toList :: (Foldable t) => t a -> [a]
toList = foldr (\x l -> x : l) []

-- Hint: use foldMap.
-- Combine the elements of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap' id

-- Define foldMap in terms of foldr.
-- For more details, see:
-- http://brianshourd.com/posts/2013-01-15-tilt-foldable.html
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty


fold'' :: (Functor t, Foldable t, Monoid m) => t m -> m
fold'' = foldMap'' id

-- See the following for more details:
-- https://hackage.haskell.org/package/base/docs/Data-Foldable.html
foldMap'' :: (Functor t, Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = fold . fmap f

-- fold [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]
-- Sum {getSum = 15}

-- fold [1, 2, 3, 4, 5 :: Sum Integer]
-- Sum {getSum = 15}

-- foldMap Sum [1, 2, 3, 4]
-- Sum {getSum = 10}

--------------------------------------------------------------------------------
-- Write Foldable instances for the following datatypes.
data Constant a b =
  Constant a

-- see the following for more details
-- http://stackoverflow.com/q/37419925/236007
-- http://stackoverflow.com/q/37468032/236007
-- http://stackoverflow.com/q/35948278/236007
--
-- Constant a b doesn't contain any `b`s, so we fold over it
-- as if it were an empty list of `b`s:
instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty
  -- also:
  -- foldr f z (Constant a) = z

--------------------
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y
  --foldr f z (Two _ y) = f y z

--------------------
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

--------------------
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f z

--------------------
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' x y z z') = f z'

--------------------
-- Thinking cap time. Write a filter function for Foldable types
-- using foldMap.
-- For more details, see
-- https://wiki.haskell.org/Foldable_and_Traversable#Some_trickier_functions:_concatMap_and_filter
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
  
main :: IO ()
main = putStrLn "Let's do some Foldable exercises!"
