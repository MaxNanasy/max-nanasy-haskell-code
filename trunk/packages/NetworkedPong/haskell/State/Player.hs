module State.Player

data PlayerState = PlayerState
    { paddleState :: PaddleState
    , score :: Integer
    } deriving(Show)

data PaddleState = PaddleState
    { yVelocity :: LinearVelocitySign
    , yPosition :: Integer
    } deriving(Show)
