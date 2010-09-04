module Control.Monad.Omega.Interface.Class where

import OmegaRPG.Player(Player)

import Control.Monad.Random

class Monad m => MonadInterface m where
    displayStats   :: Player -> m ()
    displayMessage :: String -> m ()

    promptString  :: String -> m String
    promptBool    :: String -> m Bool
    promptInteger :: String -> m Integer
