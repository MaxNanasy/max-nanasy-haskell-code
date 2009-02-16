{-# OPTIONS_GHC -Wall -fno-implicit-prelude #-}

module Language.BlooP(fromInteger, cell, (<--), (>>), Function) where

import Number.Peano

import           Prelude hiding(fromInteger)
import qualified Prelude

type Function = T -> BlooP

data BlooP = Block [BlooP]
           | Expression := Expression
           | Loop   Expression BlooP
             deriving(Show)

data Expression = Yes
                | No
                | Expression :+: Expression
                | Expression :*: Expression
                | Cell   T
                | Output
                  deriving(Show)

(<--) :: Expression -> Expression -> BlooP
(<--) = Assignment

cell :: T -> Expression
cell = Cell

class FromInteger a where
    fromInteger :: Integer -> a

instance FromInteger Expression where
    fromInteger = Nat . fromInteger

instance FromInteger T where
    fromInteger = Prelude.fromInteger
