{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Parser.Test(test, parseFile) where

import Language.Functional.Parser.Types
import Language.Functional.Parser

import Text.PrettyPrint.Leijen
import Text.ParserCombinators.Parsec

import Control.Monad
import IO

test :: FilePath -> IO ()
test file = do
  hPutDoc stdout =<< (liftM pretty $ parseFile file)
  putChar '\n'

parseFile :: FilePath -> IO Declaration
parseFile name = do
  file <- readFile name
  return $
         either
         (error . show)
         id
         (parse declaration name file)
