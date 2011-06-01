{-# LANGUAGE DeriveDataTypeable #-}

module Game.Necromancy.Server.Main (main) where

import Game.Necromancy.Server.World
import Game.Necromancy.Common

import Network
import System.IO

import Control.Concurrent
import Control.Concurrent.STM

import Data.Typeable
import Control.Exception

import Control.Monad

data QuitException = QuitException deriving (Typeable, Show)

instance Exception QuitException

main :: IO ()
main = withSocketsDo main'

main' :: IO ()
main' = do
  world <- setupWorld
  socket <- listenOn portID
  _ <- forkIO $ forever $ forkConnection world socket
  getCommands socket

forkConnection :: World -> Socket -> IO ()
forkConnection world listeningSocket = do
  (handle, _, _) <- accept listeningSocket
  _ <- forkIO $ handleHandle world handle
  forkConnection world listeningSocket

handleHandle :: World -> Handle -> IO ()
handleHandle world handle = do
  hSetBuffering handle LineBuffering
  hPutStrLn handle "What is your name?"
  name <- hGetLine handle
  hPutStrLn handle $ "Hello, " ++ name ++ "!"
  hClose handle

getCommands :: Socket -> IO ()
getCommands socket = do
  line <- getLine
  case line of
    "quit" -> quit socket
    _      -> getCommands socket

quit :: Socket -> IO ()
quit socket = sClose socket
