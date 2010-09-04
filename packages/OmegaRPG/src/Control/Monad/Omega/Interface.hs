{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Omega.Interface(module Control.Monad.Omega.Interface.Class, evalInterface, Interface) where

import Control.Monad.Omega.Interface.Class

import System.IO
import Control.Exception

import Control.Monad.Trans

newtype Interface a = Interface { evalInterface :: IO a }
    deriving(Monad, MonadIO)

withGetSet :: IO b -> (b -> IO ()) -> b -> IO a -> IO a
withGetSet get set value x = do
  old <- get
  bracket_ (set value) (set old) x

withNoEcho :: IO a -> IO a
withNoEcho = withGetSet (hGetEcho stdin) (hSetEcho stdin) False

hWithNoBuffering :: Handle -> IO a -> IO a
hWithNoBuffering h = withGetSet (hGetBuffering h) (hSetBuffering h) NoBuffering

instance MonadInterface Interface where
    displayStats   = liftIO . print
    displayMessage = liftIO . putStrLn

    promptString  prompt = liftIO (putStr prompt >> putChar ' ' >> getLine)
    promptBool    prompt = liftIO $ do
                             putStr prompt
                             putStr " [yn] "
                             let getInput = do
                                      c <- getChar
                                      case c of
                                        'y' -> return True
                                        'n' -> return False
                                        _   -> getInput 
                             val <- withNoEcho getInput
                             putChar '\n'
                             return val
    promptInteger prompt = liftIO $ do
                             putStr prompt
                             putChar ' '
                             let getInput = do
                                      s <- getLine
                                      case reads s of
                                        [(n, "")] -> return n
                                        _         -> getInput
                             getInput
