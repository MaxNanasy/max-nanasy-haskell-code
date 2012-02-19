module Main(main) where

import Data.Maybe(catMaybes)
import Data.Stream(Stream)
import qualified Data.Stream as Stream

import Network

import Random

import System.IO(Handle, hGetContents)

import Utility

main :: IO ()
main = generateCommandStream >>= putStr . showInterpretationStream . interpretCommandStream

portID :: PortID
portID = UnixSocket "/tmp/pong.sock"

generateCommandStream :: IO (Stream UserCommand)
generateCommandStream = withSocketsDo $ listenOn portID >>= getCommandsFromSocket

getCommandsFromSocket :: Socket -> IO (Stream UserCommand)
getCommandsFromSocket socket = fmap Stream.fromList $ sMergeServerConnectionResults getCommandsFromServerConnection socket

getCommandsFromServerConnection :: (Handle, HostName, PortNumber) -> IO [UserCommand]
getCommandsFromServerConnection (handle, _, _) = fmap getCommandsFromInput $ hGetContents handle

getCommandsFromInput :: String -> [UserCommand]
getCommandsFromInput = catMaybes . map readMaybe . lines

data UserCommand = SetPaddleYVelocitySign LinearVelocitySign deriving(Read, Show)

data LinearVelocitySign = Negative | Zero | Positive deriving(Enum, Read, Show)

data Interpretation = Interpretation UserCommand deriving(Show)

interpretCommandStream :: Stream UserCommand -> Stream Interpretation
interpretCommandStream = Stream.map interpretCommand

interpretCommand :: UserCommand -> Interpretation
interpretCommand = Interpretation

showInterpretationStream :: Stream Interpretation -> String
showInterpretationStream = unlines . map show . Stream.toList
