{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Omega where

import Control.Monad.Trans
import Control.Monad.Random

newtype OmegaT m a = OmegaT (RandT StdGen m a) deriving (Monad, MonadTrans, MonadRandom)

evalOmegaT :: Monad m => OmegaT m a -> StdGen -> m a
evalOmegaT (OmegaT x) = evalRandT x
