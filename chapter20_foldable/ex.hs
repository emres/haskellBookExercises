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




main :: IO ()
main = putStrLn "Let's do some Foldable exercises!"
