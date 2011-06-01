module Game.Necromancy.Server.Main (main) where

import Game.Necromancy.Server.World
import Game.Necromancy.Common

import Network

import Control.Concurrent
import IO

import Control.Monad

main :: IO ()
main = withSocketsDo main'

main' :: IO ()
main' = do
  world <- setupWorld
  listenOn portID >>= forever . forkConnection world

forkConnection :: World -> Socket -> IO ThreadId
forkConnection world listeningSocket = do
  (handle, _, _) <- accept listeningSocket
  forkIO $ handleHandle world handle

handleHandle :: World -> Handle -> IO ()
handleHandle world handle = do
  hSetBuffering handle LineBuffering
  hPutStrLn handle "What is your name?"
  name <- hGetLine handle
  hPutStrLn handle $ "Hello, " ++ name ++ "!"
