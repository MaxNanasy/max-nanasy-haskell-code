module OmegaRPG.Player(Player(..), generateRandomPlayer) where

import Control.Monad.Omega
import Control.Monad.Random

import Control.Monad

-- | A datatype representing dynamic information about the player.

data Player = Player
    { strength, constitution, dexterity, agility, intelligence, power :: Integer
    , experience, level, hitPoints, maxHitPoints, mana, maxMana       :: Integer
    , food, alignment, money                                          :: Integer
    } deriving(Read, Show)

-- | The base range for randomly generated stats.

baseStatRange :: (Integer, Integer)
baseStatRange = (4, 8)

-- | The range for additions to base range for randomly generated stats.

modStatRange  :: (Integer, Integer)
modStatRange  = (0, 5)

-- | A player with defaults for all slots, which is mostly 0s.

defaultPlayer :: Player
defaultPlayer = Player
                  { strength     = 0
                  , constitution = 0
                  , dexterity    = 0
                  , agility      = 0
                  , intelligence = 0
                  , power        = 0
                  , experience   = 0
                  , level        = 0
                  ,    hitPoints = 0
                  , maxHitPoints = 0
                  ,    mana      = 0
                  , maxMana      = 0
                  , food         = 0
                  , alignment    = 0
                  , money        = 0
                  }

generateRandomPlayer :: Monad m => OmegaT m Player
generateRandomPlayer = do
  [baseStrength, baseConstitution, baseDexterity, baseAgility, baseIntelligence, basePower] <- replicateM 6 $ getRandomR baseStatRange
  [modPhysical, modSomatic, modMental] <- replicateM 3 $ liftM2 (+) (getRandomR modStatRange) (getRandomR modStatRange)
  playerMoney <- liftM sum . replicateM 5 $ getRandomR (0, 99)
  let playerConstitution = baseConstitution + modPhysical
      playerPower        = basePower        + modMental
  return $ defaultPlayer
             { strength     = baseStrength     + modPhysical
             , constitution = playerConstitution
             , dexterity    = baseDexterity    + modSomatic
             , agility      = baseAgility      + modSomatic
             , intelligence = baseIntelligence + modMental
             , power        = playerPower
             ,    hitPoints = playerConstitution
             , maxHitPoints = playerConstitution
             ,    mana      = playerPower
             , maxMana      = playerPower
             , money        = playerMoney
             }
