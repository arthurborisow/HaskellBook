{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Chapter23 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Die
  = DieOne
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
    -- Use 'error'
    -- _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where
      go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
      go sum c@(count, dies) gen
        | sum >= n = c
        | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1, intToDie die : dies) nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> ((f . fst . g) s, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a,)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (f', s') = f s
                                        (g', s'') = g s'
                                    in (f' g', s'')

instance Monad (Moi s) where
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= g = Moi $ \s -> let (a, s') = f s
                            in runMoi (g a) s'

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
