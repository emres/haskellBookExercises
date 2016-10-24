{-# LANGUAGE InstanceSigs #-}
module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- take 6 $ evalState infiniteDie (mkStdGen 0)

-- replicateM :: Monad m => Int -> m a -> m [a]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- evalState (nDie 5) (mkStdGen 0)

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1) nextGen

-- rollsToGetTwenty (mkStdGen 0)

-- randomIO :: Random a => IO a

-- (rollsToGetTwenty . mkStdGen) <$> randomIO

-- Refactor rollsToGetTwenty into having the limit be a function argument.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1) nextGen

-- Change rollsToGetN to recording the series of die
-- that occurred in addition to the count.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, listOfDies) gen
      | sum >= n = (count, listOfDies)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1, (intToDie die) : listOfDies) nextGen

--------------------------------------------------------------------------------
newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- Implement the Functor instance for State.
instance Functor (Moi s) where
  --fmap :: (a -> b) -> Moi s a -> Moi s b
  --fmap f (Moi g) = Moi $ \s -> let (x, s') = g s
  --                             in (f x, s')
  fmap f (Moi g) = Moi $ \x -> (f (fst (g x)), snd (g x))


instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi h
    where h s = let (fab, s') = f s
                    (a, s'')  = g s'
                in (fab a, s'')
  
