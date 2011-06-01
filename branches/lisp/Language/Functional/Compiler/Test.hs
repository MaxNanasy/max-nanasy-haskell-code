{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Compiler.Test(compileFile, test) where

import Language.Functional.Compiler
import Language.Functional.Compiler.Types
import Language.Functional.Parser.Test hiding(test)

--import Text.PrettyPrint.Leijen

import IO
import Monad

test :: FilePath -> IO ()
test file = do
  runMain =<< compileFile file
--  hPutDoc stdout =<< (liftM (pretty . (runMain :: Module -> Integer)) $ compileFile file)
--  putChar '\n'

compileFile :: FilePath -> IO Module
compileFile name = liftM compile $ parseFile name
