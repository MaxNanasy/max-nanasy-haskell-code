module Data.NeuralNetwork where

import Control.Concurrent.Actor
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

data Synapse a = Synapse a a (Actor a)
data ConnectM a = Connect a (Actor a) (Actor (Actor a))
neuronB :: Num a => Actor a -> ([(a, a)] -> a) -> a -> M.Map Integer (Synapse a) -> Integer -> Behavior (ConnectM a)
neuronB output calculation defaultWeight feedbacks n (Connect initialValue feedback connectionR) = do
  let -- updateFeedbacks update :: (M.Map Integer (Synapse a) -> M.Map Integer (Synapse a)) -> Acting msg 
      updateFeedbacks update = do
        let newFeedbacks = update feedbacks
        send output $ calculation $ map (\ (Synapse x y _) -> (x, y)) $ M.elems newFeedbacks
        become $ \ (Connect initialValue feedback connectionR) -> neuronB output calculation defaultWeight newFeedbacks (n + 1) (Connect initialValue feedback connectionR)
      stimulateB newValue = updateFeedbacks $ M.adjust (\ (Synapse _ weight feedback) -> Synapse newValue weight feedback) n
--  spawn stimulateB >>= send connectionR
  updateFeedbacks $ M.insert n (Synapse initialValue defaultWeight feedback)

defaultDefaultWeight :: Num a => a
defaultDefaultWeight = 1

neuron :: Num a => Actor a -> ([(a, a)] -> a) -> Acting msg (Actor (ConnectM a))
neuron output calculation = do
  spawn $ neuronB output calculation defaultDefaultWeight M.empty 0

data StateM a = Get (Actor a)
              | Set a
stateB :: a -> Behavior (StateM a)
stateB x (Get output) = send output x
stateB _ (Set x     ) = become $ stateB x

test :: IO ()
test = do
  printer <- spawnIOActorIO putStrLn
  spawnIO (testB printer) >>= flip sendIO ()

testB :: Actor String -> Behavior ()
testB output () = do
  numberOutput <- spawn $ send output . show
  neuron <- neuron numberOutput (\ xs -> if (sum $ map (uncurry (*)) xs) > 0 then 1 else 0)
  let feedbackB x = send output $ "Neuron fed back error of " ++ show x
      connectionB stimulate = send stimulate 42
  feedback <- spawn feedbackB
  connection <- spawn connectionB
  send neuron (Connect 0 feedback connection)
