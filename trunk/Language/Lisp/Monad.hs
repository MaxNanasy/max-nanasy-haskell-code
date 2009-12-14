{-# LANGUAGE Rank2Types #-}

module Language.Lisp.Monad where

import Language.Lisp.Types

import Control.Monad.Reader
import Data.IORef

import System.IO

runLisp :: Lisp a -> IO a
runLisp c = do
  envC <- liftM Cell $ newIORef Nil
  runReaderT (unLisp c) envC

liftGlobalEnv x = Lisp x

readCell :: Cell -> Lisp Object
readCell  (Cell c) = liftIO . readIORef $ c

writeCell :: Cell -> Object -> Lisp ()
writeCell (Cell c) = liftIO . writeIORef c

newCell :: Object -> Lisp Cell
newCell x = do
  xC <- liftIO $ newIORef x
  return $ Cell xC

readCharacter :: Lisp Char
readCharacter = liftIO getChar 

peekCharacter :: Lisp Char
peekCharacter = liftIO $ hLookAhead stdin

writeCharacter :: Char -> Lisp ()
writeCharacter = liftIO . putChar

writeString :: String -> Lisp ()
writeString = liftIO . putStr

getGlobalEnvironment :: Lisp Cell
getGlobalEnvironment = liftGlobalEnv ask
