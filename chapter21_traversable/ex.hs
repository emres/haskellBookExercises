-- Write a Traversable instance for the datatype provided

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Applicative Identity  where
  pure x = Identity x
  Identity f <*> Identity x = Identity (f x)
  
instance Traversable Identity where
  --traverse f (Identity x) = Identity <$> f x
  traverse f (Identity x) = fmap Identity (f x)


main :: IO ()
main = putStrLn "Let's do some Traversable exercises!"
