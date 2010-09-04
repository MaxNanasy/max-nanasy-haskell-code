{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Control.Monad.Omega.Interface.Class where

import OmegaRPG.Player(Player)

import Control.Monad.Trans



class Monad m => MonadInterface m where
    displayStats   :: Player -> m ()
    displayMessage :: String -> m ()

    promptString  :: String -> m String
    promptBool    :: String -> m Bool
    promptInteger :: String -> m Integer

instance (Monad (t m), MonadInterface m, MonadTrans t) => MonadInterface (t m) where
    displayStats   = lift . displayStats
    displayMessage = lift . displayMessage

    promptString  = lift . promptString
    promptBool    = lift . promptBool
    promptInteger = lift . promptInteger
