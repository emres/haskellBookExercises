{-# LANGUAGE InstanceSigs #-}

newtype Identity a =
  Identity { runIdentity :: a }

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- For more details, see the following:
-- https://carlo-hamalainen.net/blog/2014/1/2/applicatives-compose-monads-do-not
-- https://www.youtube.com/watch?v=AjtQ0sQaHn0
-- http://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont
-- http://stackoverflow.com/questions/29453915/composing-monads-v-applicative-functors

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a))

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)
                                -- Or: Compose (pure (<*>) <*> f <*> a)

--------------------------------------------------------------------------------

