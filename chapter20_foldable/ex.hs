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

--minimum :: (Foldable t, Ord a) => t a -> Maybe a
--minimum l = foldr (\x y -> if x < y then x else y) 0 l
