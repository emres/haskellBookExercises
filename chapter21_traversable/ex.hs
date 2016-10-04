-- Write a Traversable instance for the datatype provided

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap _ (Identity _) = mempty

instance Applicative Identity  where
  pure = Identity
  Identity f <*> x = fmap f x
  
instance Traversable Identity where
  traverse f (Identity x) = Identity _hole2


main :: IO ()
main = putStrLn "Let's do some Traversable exercises!"
