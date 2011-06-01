module Data.Monoid.Trans where

import Data.Monoid

class MonoidTrans t where
    lift :: (Monoid m) => m -> t m

instance MonoidTrans [] where
    lift = return
