module Game.Necromancy.Client.Main (main) where

import Game.Necromancy.Common

import Network

import Control.Concurrent
import IO

main :: IO ()
main = withSocketsDo main'

main' :: IO ()
main' = do
  handle <- connectTo hostName portID
  hSetBuffering handle LineBuffering
  hSetBuffering stdin  LineBuffering

hostName :: HostName
hostName = "localhost"
