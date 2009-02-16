{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Brainfuck(runBrainfuck) where

import Prelude hiding(until)

import Control.Monad.State
import Control.Monad.Writer

import Language.Brainfuck.Class
import Data.List.Iterator

type Array = ListIterator

class    (Eq e, Enum e, MonadState (Array e) m, MonadWriter [e] m) => BC m e
instance (Eq e, Enum e, MonadState (Array e) m, MonadWriter [e] m) => BC m e

newtype Brainfuck m a = BF (m a)
    deriving(Monad)

modCell :: MonadState (Array e) m => (e -> e) -> m ()
modCell = modify . applyToNow

_setCell :: MonadState (Array e) m => e -> m ()
_setCell = modify . setNow

getCell :: MonadState (Array e) m => m e
getCell = gets now

getsCell :: MonadState (Array e) m => (e -> a) -> m a
getsCell f = getCell >>= return . f

cellIsZero :: (Eq e, Enum e, MonadState (Array e) m) => m Bool
cellIsZero = getsCell (toEnum 0 ==)

until :: Monad m => m Bool -> m a -> m ()
until test body = let loop = do
                        test' <- test
                        unless test' $
                               body >> loop
                  in loop

instance (Eq e, Enum e, MonadState (Array e) m, Monad m0, MonadWriter (m0 e) m) => MonoidBF (MonadAsMonoid (Brainfuck m ())) where
    incCell                       = MaM $ BF $ modCell succ
    decCell                       = MaM $ BF $ modCell pred
    incPointer                    = MaM $ BF $ modify  next
    decPointer                    = MaM $ BF $ modify  prev
    inputCell                     = inputCell
    outputCell                    = MaM $ BF $ getCell >>= tell . return
    loopUntilZero (MaM (BF body)) = MaM $ BF $ until cellIsZero body

runBrainfuck :: (Monoid (m e), Monad m) => MonadAsMonoid (Brainfuck (WriterT (m e) (State (Array e))) ()) -> Array e -> [e] -> m e
runBrainfuck (MaM (BF program)) array _input = evalState (execWriterT program) array
