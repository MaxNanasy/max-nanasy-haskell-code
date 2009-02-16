{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Brainfuck.Class where

import Data.Monoid

class Monoid m => MonoidBF m where
    incCell, decCell,
           incPointer, decPointer,
           inputCell, outputCell :: m
    loopUntilZero                :: m -> m

newtype MonadAsMonoid a = MaM a
instance Monad m => Monoid (MonadAsMonoid (m ())) where
    mempty = MaM $ return ()
    (MaM x) `mappend` (MaM y) = MaM $ x >> y
