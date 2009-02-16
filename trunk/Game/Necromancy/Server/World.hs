module Game.Necromancy.Server.World (setupWorld, World) where

import Data.IORef

data World = World (IORef 

setupWorld :: IO World
setupWorld = 
