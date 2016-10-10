import Data.Monoid

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
data List a =
    Nil
  | Cons a (List a)


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = (Cons (f x) (fmap f xs))

instance Foldable List where
  foldr f z (Cons x xs) = f x (foldr f z xs)
  foldr _ z Nil = z

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = fmap Cons (f x) <*> traverse f xs


--------------------------------------------------------------------------------
data Three a b c =
  Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  --traverse f (Three x y z) = Three x y <$> (f z)
  traverse f (Three x y z) = fmap (Three x y) (f z)

--------------------------------------------------------------------------------
data Three' a b =
  Three' a b b

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' _ _ z) = f z

instance Traversable (Three' a) where
  traverse f (Three' x y z) = (fmap (Three' x) (f y)) <*> (f z)

--------------------------------------------------------------------------------
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr too for extra credit.

instance Foldable Tree where
  foldMap f Empty = mempty  
  foldMap f (Node left node right) = foldMap f left <> f node <> foldMap f right

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node left node right) = Node <$> traverse f left <*> f node <*> traverse f right


--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn "Let's do some Traversable exercises!"
