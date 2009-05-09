module Data.NeuralNetwork where

import Control.Concurrent.Actor

data SynapseM a = SM (Neuron a) a

type Neuron a = Actor (SynapseM a)

test :: IO ()
test = spawnIOActorIO print >>= spawnIO . flip testB 7 >>= flip sendIO () >> let loop = loop in loop

testB :: Actor Integer -> Integer -> Behavior ()
testB printer n () = send printer 42 >> testB printer n ()
testB printer 0 () = become $ const $ return ()
--  spawn testB >>= flip send ()
--  testB ()

{-import Data.List
import Data.Monoid

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x,   y)
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd g (x, y) = (  x, g y)

newtype Neuron a = Neuron { run :: [a] -> (a, Neuron a) }
newtype Network a = Network [[Neuron a]]

makeNeuron :: ([a] -> (a, Neuron a)) -> Neuron a
makeNeuron = Neuron

runNeuron :: Neuron a -> [a] -> (a, Neuron a)
runNeuron = run

weighted :: Num a => [a] -> Neuron a -> Neuron a
weighted weights neuron = makeNeuron $ runNeuron neuron . zipWith (*) weights

thresholded :: Ord a => (a, a) -> a -> Neuron a -> Neuron a
thresholded (low, high) threshold neuron = makeNeuron $ \ inputs -> case runNeuron neuron inputs of (output, newNeuron) -> (if output < threshold then low else high, newNeuron)

runNetwork :: Network a -> [a] -> ([a], Network a)
runNetwork (Network layers) = mapSnd Network . runNetwork' layers where
    runNetwork' :: [[Neuron a]] -> [a] -> ([a], [[Neuron a]])
    runNetwork' []               inputs = (inputs, [])
    runNetwork' (layer : layers) inputs = (outputs, newNeurons : laterNeurons) where
                                                                  (newInputs, newNeurons) = unzip $ map (flip runNeuron inputs) layer
                                                                  (outputs, laterNeurons) = runNetwork' layers outputs

makeTestNeuron :: Num a => a -> Neuron a
makeTestNeuron initialValue = makeNeuron $ \ inputs -> let newValue = initialValue + sum inputs in (newValue, makeTestNeuron newValue)

test :: IO ()
test = do
  let testNeuron              = makeTestNeuron 0
      (output0, testNeuron' ) = runNeuron testNeuron  [8, -3, 2]
      (output1, testNeuron'') = runNeuron testNeuron' [8, -3, 2]
  print $ output0
  print $ output1-}

{-
type Weight = Float
data Neuron  = Neuron  { inputWeights :: [Weight]
                       , threshold    :: Weight
                       } deriving Show
data Network = Network { hiddenLayer  :: [Neuron]
                       , outputLayer  :: [Neuron]
                       } deriving Show
type TestCase = ([Weight], [Weight])

neuronOutput :: [Weight] -> Neuron -> Weight
neuronOutput inputs (Neuron weights threshold) = 1 / (1 + exp (foldl (-) threshold $ zipWith (*) inputs weights))

exec :: Network -> [Weight] -> ([Weight], [Weight])
exec (Network hiddenLayer outputLayer) inputs = (hiddenOutputs, outputOutputs) where
    hiddenOutputs = map (neuronOutput inputs       ) hiddenLayer
    outputOutputs = map (neuronOutput hiddenOutputs) outputLayer

defaultLearningRate :: Weight
defaultLearningRate = 0.1

step :: TestCase -> (Network, Weight) -> (Network, Weight)
step (inputs, expected) (network @ (Network hiddenLayer outputLayer), error) = (Network newHiddenLayer newOutputLayer, newError) where
    (hiddenOutputs, outputOutputs) = exec network inputs
    errorOutputs = zipWith (-) expected outputOutputs
    gradientOutputs = zipWith (\ o d -> o * (1 - o) * d) outputOutputs errorOutputs
    newOutputLayer = zipWith (\ (Neuron ws t) g -> Neuron (zipWith (\ h w -> w + defaultLearningRate * h * g) hiddenOutputs ws) (t - defaultLearningRate * g)) outputLayer gradientOutputs
    weightsByHidden = transpose $ map inputWeights outputLayer
    gradientHidden  = zipWith (\ o ws -> o * (1 - o) * (sum $ zipWith (*) ws gradientOutputs)) hiddenOutputs weightsByHidden
    newHiddenLayer = zipWith (\ (Neuron ws t) g -> Neuron (zipWith (\ h w -> w + defaultLearningRate * h * g) inputs        ws) (t - defaultLearningRate * g)) hiddenLayer gradientHidden
    newError = error + sum (map (^ 2) errorOutputs)

epoch :: Network -> [TestCase] -> (Network, Weight)
epoch network = foldr step (network, 0)

errorConvergence :: Weight
errorConvergence = 0.001

maxEpochNb :: Int
maxEpochNb = 100000

run :: Network -> [TestCase] -> Int -> (Network, Int, Weight)
run network allInputs epochNb = let (newNetwork, delta) = epoch network allInputs
                                in if delta <= errorConvergence || epochNb > maxEpochNb
                                   then (newNetwork, epochNb, delta)
                                   else run newNetwork allInputs (epochNb + 1)

test :: IO ()
test = do
  let n = Network [Neuron [0.5, 0.4] 0.8, Neuron [0.9, 1.0] (-0.1)] [Neuron [-1.2, 1.1] 0.3]
  let (n',e,err) = run n [([1,1],[0]),([0,1],[1]),([1,0],[1]),([0,0],[0])] 1
  print (n',e,err)
-}
