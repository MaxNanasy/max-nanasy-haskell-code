module Utility.Moria.RandCS where

import Control.Monad.Random
import Control.Monad

data Race               = Human | HalfElf | Elf | Halfling | Gnome | Dwarf | HalfOrc | HalfTroll
                          deriving (Show, Enum, Bounded)
data Sex                = Male | Female
                          deriving (Show, Enum, Bounded)
newtype RefusalCount    = RC Integer
    deriving (Show)
data Class              = Warrior | Mage | Priest | Rogue | Ranger | Paladin
                          deriving (Show, Enum, Bounded)
newtype Name            = Name String
    deriving (Show)

data CharacterSelection = CS Race Sex RefusalCount Class Name
                          deriving (Show)

instance Random CharacterSelection where
    random = runRand getRandomCS

getRandomCS :: MonadRandom m => m CharacterSelection
getRandomCS = do
  return CS `ap` getRandom `ap` getRandom `ap` getRandom `ap` getRandom `ap` getRandom

instance Random Race where
    random = randomEnum
    randomR = randomREnum

instance Random Sex where
    random = randomEnum
    randomR = randomREnum

instance Random RefusalCount where
    random = runRand getRandomRC

instance Random Class where
    random = randomEnum
    randomR = randomREnum

instance Random Name where
    random = runRand $ liftM Name $ liftM2 take (getRandomR (0, 16)) (getRandomRs (' ', '~'))

getRandomRC :: MonadRandom m => m RefusalCount
getRandomRC = liftM RC getRandomRCInteger

getRandomRCInteger :: MonadRandom m => m Integer
getRandomRCInteger = do
  accept <- getRandom
  if accept then return 0 else liftM succ getRandomRCInteger

randomEnum :: (RandomGen g, Enum a, Bounded a) => g -> (a, g)
randomEnum = randomREnum (minBound, maxBound)

randomREnum :: (RandomGen g, Enum a) => (a, a) -> g -> (a, g)
randomREnum (x, y) g = case randomR (fromEnum x, fromEnum y) g of
                         (z, g') -> (toEnum z, g')
