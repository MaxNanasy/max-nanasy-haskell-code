module Language.Lisp.Input (read, readChar, peekChar) where

import Language.Lisp.Monad
import Language.Lisp.Symbol
import Language.Lisp.List

import Data.Maybe

import Control.Monad.Trans
import System.IO

import Prelude hiding (read)

readChar :: Stream -> Lisp (Maybe Char)
readChar stream = liftIO $ do
                    eof <- hIsEOF stream
                    if eof then return Nothing else liftM Just $ hGetChar stream

peekChar :: Stream -> Lisp (Maybe Char)
peekChar stream = liftIO $ do
                    eof <- hIsEOF stream
                    if eof then return Nothing else liftM Just $ hLookAhead stream

isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\n")

read :: Stream -> Lisp Object
read stream = do
  cc <- readChar stream
  case cc of
    Nothing -> return Nil
    Just  c -> case c of
                 '(' -> readDelimitedList stream ')' '.'
                 '#' -> do
                        cM <- readChar stream
                        case cM of
                          Just '\\' -> liftM (Char . fromJust) $ readChar stream
                          Just c'   -> error $ c' : " RAPE"
                          Nothing   -> error "EOF RAPE"
                 _ | isWhitespace      c -> read stream
                   | isSymbolCharacter c -> do
                        token <- readToken stream
                        intern $ c : token
                   | otherwise           -> error $ "RAPE\nby #\\" ++ c : []

skipWhitespace :: Stream -> Lisp ()
skipWhitespace stream = do
  cc <- peekChar stream
  case cc of
    Just c | isWhitespace c -> readChar stream >> skipWhitespace stream
    _                       -> return ()

isSymbolCharacter :: Char -> Bool
isSymbolCharacter = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/")

readToken :: Stream -> Lisp String
readToken stream = do
  cM <- peekChar stream
  case cM of
    Just c | isSymbolCharacter c -> do readChar stream
                                       token <- readToken stream
                                       return $ c : token
    _                            -> return ""

readDelimitedList :: Stream -> Char -> Char -> Lisp Object
readDelimitedList stream c d = do
  skipWhitespace stream
  Just c' <- peekChar stream
  if c' == c
    then do
      readChar stream
      return Nil
    else if c' == d
         then do
           readChar stream
           Cons objectC _ <- readDelimitedList stream c d
           readCell objectC
         else do
           object <- read stream
           rest <- readDelimitedList stream c d
           cons object rest
