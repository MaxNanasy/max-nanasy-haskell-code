{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Brainfuck(runBrainfuck) where

import Prelude hiding(until)

import Control.Monad.State
import Control.Monad.Consumer
import Control.Monad.Writer

import Language.Brainfuck.Class

import Data.Stream.Iterator

type Array = StreamIterator

class    (Eq e, Enum e, MonadState (Array e) m, MonadConsumer e m, MonadWriter [e] m) => BC e m
instance (Eq e, Enum e, MonadState (Array e) m, MonadConsumer e m, MonadWriter [e] m) => BC e m

newtype Brainfuck m = BF (MonadAsMonoid (m ()))
    deriving(Monoid)

modCell :: BC e m => (e -> e) -> m ()
modCell = modify . applyToCurrent

setCell :: BC e m => e -> m ()
setCell = modify . setCurrent

getCell :: BC e m => m e
getCell = gets current

getsCell :: BC e m => (e -> a) -> m a
getsCell f = getCell >>= return . f

cellIsZero :: BC e m => m Bool
cellIsZero = getsCell (toEnum 0 ==)

until :: Monad m => m Bool -> m a -> m ()
until test body = let loop = do
                        test' <- test
                        unless test' $
                             body >> loop
                  in loop

instance BC e m => MonoidBF (Brainfuck m) where
    incCell                       = BF $ MaM $ modCell succ
    decCell                       = BF $ MaM $ modCell pred
    incPointer                    = BF $ MaM $ modify  next
    decPointer                    = BF $ MaM $ modify  prev
    inputCell                     = BF $ MaM $ consume >>= setCell
    outputCell                    = BF $ MaM $ getCell >>= tell . return
    loopUntilZero (BF (MaM body)) = BF $ MaM $ until cellIsZero body

runBrainfuck :: (Enum e, Monoid (m e)) => Brainfuck (StateT (Array e) (WriterT (m e) (Consumer e))) -> Stream e -> m e
runBrainfuck (BF (MaM program)) = evalConsumer . execWriterT . evalStateT program $ finiteListToStreamIterator [toEnum 0]
