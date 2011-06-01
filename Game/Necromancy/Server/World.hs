module Game.Necromancy.Server.World (setupWorld, World) where

import Data.IORef

data World = World

setupWorld :: IO World
setupWorld = return World
