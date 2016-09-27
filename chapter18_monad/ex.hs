import Control.Monad (join)


-- fmap :: Functor     f =>   (a -> b) -> f a        -> f b
-- <*>  :: Applicative f => f (a -> b) -> f a        -> f b
-- >>=  :: Monad       f => f a        -> (a -> f b) -> f b

-- The answer is the exercise.
-- Write bind in terms of fmap and join.
-- Fear is the mind-killer, friend. You can do it.
-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
   } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
  then Nothing
  else Just c

-- Desugared form?:
-- mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow name' age' weight' =
--   case noEmpty name' of
--     Nothing -> Nothing
--       Just nammy -> case noNegative age' of
--           Nothing -> Nothing
--            Just agey ->
--              case noNegative weight' of
--                Nothing -> Nothing
--                Just weighty ->
--                  weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

--------------------------------------------------------------------------------
-- m ~ Either e
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Either e a -> (a -> Either e b) -> Either e b

-- same as pure
-- return :: Monad m => a -> m a
-- return :: a -> Either e a

-- Implement the Either monad
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure x = (Second x)
  (Second f) <*> (Second x)  = Second (f x)
  (First  f) <*> (Second _)  = First f
  _          <*> (First  x)  = First x
  
instance Monad (Sum a) where
  return = pure
  (Second x) >>=  f = f x
  (First  x) >>=  _ = First x

--------------------------------------------------------------------------------
-- Write a Monad instance for Identity.
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure  x = Identity x 
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return           = pure
  Identity x >>= f = f x
  
--------------------------------------------------------------------------------
-- Write the following functions using the methods provided by Monad and
-- Functor. Using stuff like identity and composition is fine, but it has to
-- typecheck with types provided.

j :: Monad m => m (m a) -> m a
j = join
-- Expecting the following behavior:

--      Prelude> j [[1, 2], [], [3]]
--      [1,2,3]
--      Prelude> j (Just (Just 1))
--      Just 1
--      Prelude> j (Just Nothing)
--      Nothing
--      Prelude> j Nothing
--      Nothing


