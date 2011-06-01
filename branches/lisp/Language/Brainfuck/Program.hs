{-# LANGUAGE RecursiveDo #-}

module Language.Brainfuck.Program (main) where

import Language.Brainfuck
import Language.Brainfuck.Parser

import Data.Stream

import Data.Char

import System.IO
import System.Console.ParseArgs hiding (args)

import Control.Monad

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete possibleArgs
  if gotArg args Help
    then putStrLn $ argsUsage args ++ description
    else mdo
      let file = getRequiredArg args CodeFile
      code <- readFile file
      zipWithM_ hSetBuffering [stdin, stdout] [LineBuffering, LineBuffering]
      input <- getContents
      let interpretOutput :: Integer -> IO String
          interpretOutput n | n' >= ord minBound && n' <= ord maxBound = do putChar c ; return "" where n' = fromInteger n ; c = chr n'
          interpretOutput (-1) = return "a"
      output <- liftM concat . sequence . map interpretOutput $ runBrainfuckCode (if gotArg args Script then dropWhile (/= '\n') code else code) input
      print output

data Option = Help | Script | CodeFile deriving (Eq, Ord, Show)

possibleArgs :: [Arg Option]
possibleArgs = [ Arg { argIndex = Help      , argName = Just "help"  , argAbbr = Just 'h', argData = Nothing                                  , argDesc = "usage information"                                                }
               , Arg { argIndex = Script    , argName = Just "script", argAbbr = Just 's', argData = Nothing                                  , argDesc = "treat code file as an interpreted script if '#!' line is present" }
               , Arg { argIndex = CodeFile  , argName = Nothing      , argAbbr = Nothing , argData = argDataRequired "code file" ArgtypeString, argDesc = "file with Brainfuck code"                                         }
               ]

description :: String
description = "BRAINFUCK"

class EOF a where
    eof :: a

instance EOF Integer where
    eof = -1

makeListWithEOF :: EOF a => [a] -> [a]
makeListWithEOF = (++ repeat eof)

runBrainfuckCode :: String -> String -> [Integer]
runBrainfuckCode source = runBrainfuck (parseSource source) . listToStream . makeListWithEOF . map (toInteger . ord)
