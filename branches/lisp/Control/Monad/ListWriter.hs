{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.ListWriter where

import Control.Monad.State

newtype ListWriter e a = ListWriter (State [e] a)
    deriving(Monad, MonadState [e])

write :: e -> ListWriter e ()
write = modify . (:)

runListWriter :: ListWriter e a -> (a, [e])
runListWriter (ListWriter lw) = runState lw []

execListWriter :: ListWriter e a -> [e]
execListWriter = snd . runListWriter

newtype ListWriter' e a = ListWriter' (a, [e] -> [e])

instance Monad (ListWriter' e) where
    return x = ListWriter' (x, id)
    ListWriter' (x, f) >>= b = ListWriter' (x', f . f') where
                               ListWriter' (x', f') = b x

write' :: e -> ListWriter' e ()
write' x = ListWriter' ((), (x:))

runListWriter' :: ListWriter' e a -> (a, [e])
runListWriter' (ListWriter' (x, f)) = (x, f [])

execListWriter' :: ListWriter' e a -> [e]
execListWriter' = snd . runListWriter'
