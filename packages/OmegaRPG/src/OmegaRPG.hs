module Main(main) where

import OmegaRPG.Player
import Control.Monad.Omega
import Control.Monad.Omega.Interface

import Random

main :: IO ()
main = getStdGen >>= evalInterface . evalOmegaT omegaRPG

omegaRPG :: MonadInterface m => OmegaT m ()
omegaRPG = roll >>= displayStats

roll :: MonadInterface m => OmegaT m Player
roll = do
  player <- generateRandomPlayer
  displayStats player
  keep <- promptBool "Keep this player?"
  if keep then return player else roll
