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
