{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Compiler.Test(compileFile, test) where

import Language.Functional.Compiler
import Language.Functional.Parser.Test hiding(test)

import Text.PrettyPrint.Leijen

import Monad
import IO

test :: FilePath -> IO ()
test file = do
  hPutDoc stdout =<< (liftM pretty $ compileFile file)
  putChar '\n'

compileFile :: FilePath -> IO Environment
compileFile name = liftM compileModule $ parseFile name
