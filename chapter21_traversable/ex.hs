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

--------------------------------------------------------------------------------
newtype Constant a b =
  Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

--------------------------------------------------------------------------------
data Optional a =
    Nada
  | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = fmap Yep (f x)


--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn "Let's do some Traversable exercises!"
