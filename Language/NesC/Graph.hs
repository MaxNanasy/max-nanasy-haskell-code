module Language.NesC.Graph where

import Language.NesC.Types

import Control.Monad.Writer.Lazy

graphConfiguration :: Configuration -> Writer String ()
graphConfiguration (Configuration name componentDeclarations wirings) = do
  tell "digraph "
  tell name
  tell "\n{\n"
  mapM_ graphComponentDeclaration componentDeclarations
  mapM_ graphWiring wirings
  tell "}\n"

graphComponentDeclaration :: ComponentDeclaration -> Writer String ()
graphComponentDeclaration (CD (CI name _) Nothing) = do
  tell name
  tell ";\n"
graphComponentDeclaration (CD (CI _ _) (Just name)) = do
  tell name
  tell ";\n"

graphWiring :: Wiring -> Writer String ()
graphWiring ((Component a _) :-> (Component b _)) = do
  tell a
  tell " -> "
  tell b
  tell ";\n"
