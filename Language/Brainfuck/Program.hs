module Language.Brainfuck.Program (main) where

import Language.Brainfuck
import Language.Brainfuck.Parser

import Data.Stream

import Data.Char

import System.IO
import System.Console.ParseArgs

import Control.Monad

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete possibleArgs
  if gotArg args Help
    then putStrLn $ argsUsage args ++ description
    else do
      let file = getRequiredArg args CodeFile
      code <- readFile file
      zipWithM_ hSetBuffering [stdin, stdout] [LineBuffering, LineBuffering]
      interact $ runBrainfuckCode $ if gotArg args Script then dropWhile (/= '\n') code else code

data Option = Help | Script | CodeFile deriving (Eq, Ord, Show)

possibleArgs :: [Arg Option]
possibleArgs = [ Arg { argIndex = Help  , argName = Just "help"  , argAbbr = Just 'h', argData = Nothing, argDesc = "usage information" }
               , Arg { argIndex = Script, argName = Just "script", argAbbr = Just 's', argData = Nothing, argDesc = "treat code file as an interpreted script if '#!' line is present" }
               , Arg { argIndex = CodeFile  , argName = Nothing      , argAbbr = Nothing , argData = argDataRequired "code file" ArgtypeString, argDesc = "file with Brainfuck code" }
               ]

description = "BRAINFUCK"

class EOF a where
    eof :: a

instance EOF Integer where
    eof = -1

makeListWithEOF :: EOF a => [a] -> [a]
makeListWithEOF = (++ repeat eof)

runBrainfuckCode :: String -> String -> String
runBrainfuckCode source = map (chr . max minBound . fromInteger) . runBrainfuck (parseSource source) . listToStream . makeListWithEOF . map (toInteger . min maxBound . ord)
