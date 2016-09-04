{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- CHAPTER 12, "Signaling Adversity" Exercises below !!! --

-- Use the Maybe type to write a function that counts the number of vowels
-- in a string and the number of consonants. If the number of vowels exceeds
-- the number of consonants, the function returns Nothing. In many human
-- languages, vowels rarely exceed the number of consonants so when they do,
-- it indicates the input isn’t a real word (that is, a valid input to
-- your dataset)       
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = if (sum vowelAndConsonantEncoding) > numConsonants
             then Nothing
             else Just (Word' str)
  where
    numConsonants= length $ filter (\x -> x == 0) vowelAndConsonantEncoding
    vowelAndConsonantEncoding =
          map (\x -> if (elem x vowels) then 1 else 0) str


data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

 -- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x = if x > 0
                 then Just (Succ (maybe Zero id (integerToNat (x - 1))))
                 else Nothing


-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _      = False

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing  = x
mayybee _ f (Just x) = f x

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultValue Nothing  = mayybee defaultValue id Nothing
fromMaybe _            (Just x) = mayybee x id (Just x)


-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe _      = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe x = if (elem Nothing x)
              then Nothing
              else (Just (catMaybes x))

-- Write each of the following functions. If more than one
-- possible unique function exists for the type, use common
-- sense to determine what it should do.
-- 1. Try to eventually arrive at a solution that uses foldr,
-- even if earlier versions don’t use foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left x) = (x:)
        f _        = id

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right x) = (x:)
        f _         = id

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _         = Nothing


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Right x) = Just (either' id f (Right x))
eitherMaybe'' _ _  = Nothing

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


-- A very poor attempt, not making use of pattern matching
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f x | isJust $ (f x) = (fst $ head $ maybeToList (f x)) : myUnfoldr f (snd $ head $ maybeToList (f x))
--              | otherwise = []   

-- Let's use pattern matching
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (a, b) -> a : myUnfoldr f b  
  Nothing     -> []

-- Or let's use guards
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f x
--   | Just (a, b) <- f x = a : myUnfoldr f b  
--   | otherwise                = []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x

-- Write unfold for BinaryTree
unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f x = case f x of
  Just (l, n, r) -> Node (unfoldTree f l) n (unfoldTree f r)
  Nothing         -> Leaf


treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n = unfoldTree (\x -> if x < n 
                                then Just(2^x, x, 2^x)
                                else Nothing) 0

-- λ> treeBuild 3
-- Node (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))

