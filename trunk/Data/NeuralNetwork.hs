{-# LANGUAGE ScopedTypeVariables, PatternSignatures #-}

module Data.NeuralNetwork where

import Control.Concurrent.Actor
import Graphics.HGL
import qualified Data.Map as M

{-                                                       
                                                         
                                                         
              O                                          
               \                                         
                \                                        
                 \                                       
                  \
                   \                                     
                    V
                    
              O --> O --> O
              
              
              
              
              
              
              
 -}

data MPInput = Ineffect | Excitation | Inhibition deriving (Show)

data ChangeM a = Change (a -> a) (Actor a)
changeCell :: a -> Behavior (ChangeM a)
changeCell x (Change f r) = do
  let x' = f x
  send r x'
  become $ changeCell x'

data Synapse a = Synapse { synapseValue :: a , synapseFeedback :: Actor a }
data ConnectM a b = Connect a (Actor a) (Actor (Actor a))

neuronB :: forall a b. Actor b -> ([a] -> b) -> Actor (ChangeM (M.Map Integer (Synapse a))) -> Integer -> Behavior (ConnectM a b)
neuronB output calculation feedbacks n (Connect initialValue feedback connectionR) = do
  let feedbackHandlerB = send output . calculation . map synapseValue . M.elems
  feedbackHandler <- spawn feedbackHandlerB
  let updateFeedbacks :: (M.Map Integer (Synapse a) -> M.Map Integer (Synapse a)) -> Acting msg ()
      updateFeedbacks update = do
        send feedbacks $ Change update feedbackHandler
      stimulateB newValue = updateFeedbacks $ M.adjust (\ synapse -> synapse { synapseValue = newValue }) n
  spawn stimulateB >>= send connectionR
  updateFeedbacks $ M.insert n $ Synapse initialValue feedback
  become $ neuronB output calculation feedbacks (n + 1)

defaultDefaultWeight :: Num a => a
defaultDefaultWeight = 1

mpNeuron :: Actor Bool -> Integer -> Acting msg (Actor (ConnectM MPInput Bool))
mpNeuron output threshold = neuron output $ maybe False (>= threshold) . foldr f (Just 0) where
    f Ineffect   = id
    f Excitation = fmap (1 +)
    f Inhibition = const Nothing

neuron :: Actor b -> ([a] -> b) -> Acting msg (Actor (ConnectM a b))
neuron output calculation = do
  feedbacks <- spawn $ changeCell M.empty
  spawn $ neuronB output calculation feedbacks 0

excite  :: Actor MPInput -> Behavior Bool
excite  actor True  = send actor Excitation
excite  actor False = send actor Ineffect

inhibit :: Actor MPInput -> Behavior Bool
inhibit actor True  = send actor Inhibition
inhibit actor False = send actor Ineffect

data StateM a = Get (Actor a)
              | Set a
stateB :: a -> Behavior (StateM a)
stateB x (Get output) = send output x
stateB _ (Set x     ) = become $ stateB x

test :: IO ()
test = do
  printer <- spawnIOActorIO putStrLn
  runGraphics $ withWindow "Test" (200, 200) $ \ w -> do
  let drawerB = setGraphic w
  drawer <- spawnIOActorIO drawerB
  spawnIO (testB printer drawer) >>= flip sendIO ()
  getKey w
  return ()

testB :: Actor String -> Actor Graphic -> Behavior ()
testB stringOutput graphicOutput () = do
  valueOutput <- spawn $ \ x -> do send stringOutput $ show x ; send graphicOutput $ text (0, 0) $ show x
  neuron :: Actor (ConnectM MPInput Bool) <- mpNeuron valueOutput 0
  let feedbackB x = send stringOutput $ "Neuron fed back error of " ++ show x
      connectionB stimulate = send stimulate Inhibition
  feedback <- spawn feedbackB
  connection <- spawn connectionB
  send neuron (Connect Ineffect feedback connection)
  send neuron (Connect Ineffect feedback connection)
