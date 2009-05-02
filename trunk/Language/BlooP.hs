{-# OPTIONS_GHC -Wall -fno-implicit-prelude #-}

module Language.BlooP(fromInteger, cell, (<--), (>>), Function) where

import qualified Number.Peano as Peano

import           Prelude hiding(fromInteger)
import qualified Prelude

type Function = Peano.T -> BlooP

data BlooP = Block [BlooP]
           | Expression := Expression
           | Loop   Expression BlooP
             deriving(Show)

data Expression = Yes
                | No
                | Expression :+: Expression
                | Expression :*: Expression
                | Cell   Peano.T
                | Output
                  deriving(Show)

(<--) :: Expression -> Expression -> BlooP
(<--) = Assignment

cell :: Peano.T -> Expression
cell = Cell

class FromInteger a where
    fromInteger :: Integer -> a

instance FromInteger Expression where
    fromInteger = Nat . fromInteger

instance FromInteger Peano.T where
    fromInteger = Prelude.fromInteger
