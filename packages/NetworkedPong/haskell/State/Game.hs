module State.Game

data GameState = GameState
    { leftPlayerState, rightPlayerState :: PlayerState
    , ballState :: BallState
    } deriving(Show)

data BallState = BallState
    { xVelocity, yVelocity :: LinearVelocitySign
    , xPosition, yPosition :: Integer
    }
