module Game.Necromancy.Common where

import Network

portID :: PortID
portID = UnixSocket "/tmp/necromancy.sock"
