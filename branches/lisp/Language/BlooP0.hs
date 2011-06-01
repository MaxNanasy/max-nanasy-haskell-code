{-# LANGUAGE ExistentialQuantification, Rank2Types, GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Language.BlooP(runBlooP, loop, cell, getOutput, setOutput, val, (<--), T, fromInteger, (>>=), (>>), fail, ($), (+), (*)) where

import Number.Peano

import Control.Monad.ST.Lazy
import Data.STRef.Lazy

import Control.Monad.State
import Control.Monad.Reader

import Control.Monad.Cont

import Data.List

import Prelude hiding((-))

default(T)

type BlooP s o = ReaderT (RefStream s T) (StateT o (ST s))
type Cell s = STRef s T

data RefStream s a = Cons (STRef s a) (ST s (RefStream s a))

runBlooP :: BlooPType o => (forall s. BlooP s o ()) -> o
runBlooP b = runST $ execStateT (do
               cells <- lift refStream
               runReaderT b cells)
             outputDefault

class BlooPType a where
    outputDefault :: a

infix 0 <--

(<--) :: STRef s a -> a -> BlooP s o ()
(<--) = ((lift . lift) .) . writeSTRef

val :: STRef s a -> BlooP s o a
val = lift . lift . readSTRef

instance BlooPType Bool where outputDefault = False
instance BlooPType T    where outputDefault = Zero

cell :: T -> BlooP s o (Cell s)
cell n = do
  ref <- asks $ index n
  lift $ lift $ ref

index :: T -> RefStream s a -> ST s (STRef s a)
index  Zero    (Cons r _ ) = return r
index (Succ n) (Cons _ rs) = do
  rs'  <- rs
  index n rs'

refStream :: ST s (RefStream s a)
refStream = do
  ref  <- newSTRef undefined
  return $ Cons ref refStream

genericReplicateM_ :: (Monad m, Integral i) => i -> m a -> m ()
genericReplicateM_ = (sequence_ .) . genericReplicate

loop :: T -> BlooP s o a -> BlooP s o ()
loop = genericReplicateM_

getOutput :: BlooP s o o
getOutput = get

setOutput :: o -> BlooP s o ()
setOutput = put

block :: ((a -> Cont a b) -> Cont a a) -> a
block b = (`runCont` id) $ callCC b
