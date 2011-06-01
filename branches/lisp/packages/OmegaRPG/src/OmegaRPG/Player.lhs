> module OmegaRPG.Player where
> 
> import System.Random
> import Control.Monad.State
>

A datatype representing dynamic information about the player.

> data Player = Player
>     { strength, constitution, dexterity, agility, intelligence, power :: Integer
>     , experience, level, hitPoints, maxHitPoints, mana, maxMana       :: Integer
>     , food, alignment, money                                          :: Integer
>     } deriving(Read, Show)
> 
> randR :: (RandomGen g, Random a) => (a, a) -> State g a
> randR = State . randomR
> 
> baseStatRange, modStatRange :: (Integer, Integer)

The base range for randomly generated stats.

> baseStatRange = (4, 8)

The range for additions to base range for randomly generated stats.

> modStatRange  = (0, 5)
> 
> generateInitialPlayer :: RandomGen g => State g Player
> generateInitialPlayer = do
>   [baseStrength, baseConstitution, baseDexterity, baseAgility, baseIntelligence, basePower] <- replicateM 6 $ randR baseStatRange
>   [modPhysical, mod, modMental] <- replicateM 3 $ liftM 2 (+) (randR modStatRange) (randR modStatRange)
>   
>   return $ Player
>              { strength     = baseStrength     + modPhysical
>              , constitution = baseConstitution + modPhysical
>              , dexterity    = baseDexterity    + modDexterity
>              , agility      = baseAgility      + modDexterity
>              , intelligence = baseIntelligence + modMental
>              , power        = basePower        + modMental
>              }
> 
