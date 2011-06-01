{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Sound.Mplayer(main) where

import System.Process
import System.IO
import Data.Maybe
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IPC
import qualified Data.Queue as DQ
import Control.Concurrent.STM
import Data.Binary
import Data.Generics

type Process = (Handle, Handle, Handle, ProcessHandle)
type ArgList = [String]
type Environment = [(String, String)]

main :: IO ()
main =
    withMplayer
    (\ (mpIn, mpOut, mpErr, _mpHandle) ->
         do
           forkIO $ logToFile mpErr "/var/log/mplayer/errors.log"
           hSetBuffering stdin NoBuffering
           hSetEcho stdin False
           keys <- getContents
           ch <- channelAcceptSimple "/tmp/mplayer.sock"
           userInput <- inputChannelToList ch
           mplayerFeedbackString <- hGetContents mpOut
           inputs <- mergeIO (map Left userInput) (map Right $ processMplayerFeedbackString mplayerFeedbackString)
           hPutStr mpIn $ processMplayer inputs
--           hPutStr mpIn $ processMplayer (Left (Play "/root/Music/no-conclusion.mp3"):map Left (processKeys keys))
           getLine
           return ())

inputChannelToList :: TVar (DQ.Queue DQ.Input a) -> IO [a]
inputChannelToList ch = do
  x  <- recv ch
  xs <- inputChannelToList ch
  return (x : xs)

processMplayer  :: [Either UserCommand MplayerFeedback] -> String
processMplayer = concat . map (either processUserCommand processMplayerFeedback)

processUserCommand :: UserCommand -> String
processUserCommand Pause       = playOrPause
processUserCommand Quit        = quit
processUserCommand (Play file) = play file

processMplayerFeedback :: MplayerFeedback -> String
processMplayerFeedback No = undefined

processMplayerFeedbackString :: String -> [MplayerFeedback]
processMplayerFeedbackString = processMplayerFeedbackString

data UserCommand     = Pause
                     | Quit
                     | Play String
                       deriving(Show, Typeable, Data)

instance Binary Sound.Mplayer.UserCommand where
  put Pause = putWord8 0
  put Quit = putWord8 1
  put (Play a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Pause
      1 -> return Quit
      2 -> get >>= \a -> return (Play a)
      _ -> fail "no parse"

data MplayerFeedback = No

processKeys :: String -> [UserCommand]
processKeys = catMaybes . map (flip lookup keyToCommand)

keyToCommand :: [(Char, UserCommand)]
keyToCommand =  [(' ' , Pause)
                ,('q' , Quit )
                ]

withInteractiveProcess :: FilePath -> ArgList -> Maybe FilePath -> Maybe Environment -> (Process -> IO a) -> IO a
withInteractiveProcess path args cwd env =
    bracket
      (runInteractiveProcess path args cwd env)
      (\ (procIn, procOut, procErr, procHandle) ->
           do
             mapM_ hClose [procIn, procOut, procErr]
             waitForProcess procHandle)

withMplayer :: (Process -> IO a) -> IO a
withMplayer body =
    withInteractiveProcess "mplayer" mplayerOptions Nothing Nothing
    (\ mpProcess@(mpIn, mpOut, mpErr, _mpHandle) ->
         do
           mapM_ (flip hSetBuffering LineBuffering) [mpIn, mpOut, mpErr]
           bracket_
             (replicateM_ 4 $ hGetLine mpOut)
             (hPutStr mpIn $ '\n':quit)
             (body mpProcess))

mplayerOptions :: [String]
mplayerOptions = ["-slave", "-idle", "-msglevel", "all=-1:global=4"]

quit :: String
quit = "quit\n"

play :: FilePath -> String
play = ("loadfile \"" ++) . (++ "\"\n")

playOrPause :: String
playOrPause = "pause\n"

hInteract :: Handle -> Handle -> (String -> String) -> IO ()
hInteract hIn hOut f =
    do
      s <- hGetContents hIn
      hPutStr hOut $ f s

logToFile :: Handle -> FilePath -> IO ()
logToFile hand path =
    do
      loggee <- hGetContents hand
      appendFile path loggee
