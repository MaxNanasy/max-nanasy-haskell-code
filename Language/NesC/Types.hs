module Language.NesC.Types where

data Configuration = Configuration String [ComponentDeclaration] [Wiring] deriving (Show)

data Wiring = Component :-> Component deriving (Show)

data ComponentDeclaration = CD ComponentImport (Maybe String) deriving (Show)
data ComponentImport = CI String (Maybe [String]) deriving (Show)

data Component = Component String (Maybe String) deriving (Show)
