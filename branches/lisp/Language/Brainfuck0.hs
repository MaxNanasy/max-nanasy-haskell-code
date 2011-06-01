{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.Brainfuck0 (runBrainfuck) where

import Prelude hiding (until)

import Control.Monad.State
import Control.Monad.Consumer
import Control.Monad.Writer

import Control.Applicative

import Language.Brainfuck.Class

import Data.Stream.Iterator

import Data.Char

type Array = StreamIterator

class    (MonadState (Array Integer) m, MonadConsumer Integer m, MonadWriter (f Char) m, Applicative f) => BC f m
instance (MonadState (Array Integer) m, MonadConsumer Integer m, MonadWriter (f Char) m, Applicative f) => BC f m

newtype Brainfuck m = BF (MonadAsMonoid (m ()))
    deriving(Monoid)

modCell :: BC f m => (Integer -> Integer) -> m ()
modCell = modify . applyToCurrent

setCell :: BC f m => Integer -> m ()
setCell = modify . setCurrent

getCell :: BC f m => m Integer
getCell = gets current

getsCell :: BC f m => (Integer -> a) -> m a
getsCell f = getCell >>= return . f

cellIsZero :: BC f m => m Bool
cellIsZero = getsCell (zero ==)

interpretOutput :: BC f m => Integer -> m ()
interpretOutput n | n' >= ord minBound && n' <= ord maxBound = tell $ pure c
                  | n' > ord maxBound = return ()                            where n' = fromInteger n ; c = chr n'
interpretOutput (-1) = return ()

while :: Monad m => m Bool -> m a -> m ()
while test body = let loop = do
                        test' <- test
                        when test' $
                             body >> loop
                  in loop

until :: Monad m => m Bool -> m a -> m ()
until = while . liftM not

zero :: Enum a => a
zero = toEnum 0

instance BC f m => MonoidBF (Brainfuck m) where
    incCell                       = BF $ MaM $ modCell succ
    decCell                       = BF $ MaM $ modCell pred
    incPointer                    = BF $ MaM $ modify  next
    decPointer                    = BF $ MaM $ modify  prev
    inputCell                     = BF $ MaM $ consume >>= setCell
    outputCell                    = BF $ MaM $ getCell >>= interpretOutput
    loopUntilZero (BF (MaM body)) = BF $ MaM $ until cellIsZero body

runBrainfuck :: Monoid (f Char) => Brainfuck (StateT (Array Integer) (WriterT (f Char) (Consumer Integer))) -> Stream Integer -> f Char
runBrainfuck (BF (MaM program)) = evalConsumer . execWriterT . evalStateT program $ finiteListToStreamIterator [zero]
