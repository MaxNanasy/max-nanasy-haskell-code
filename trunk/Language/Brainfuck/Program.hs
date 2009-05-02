module Language.Brainfuck.Program (main) where

import Language.Brainfuck
import Language.Brainfuck.Parser

import Data.Stream

import Data.Char

import System.IO
import System.Environment

import Control.Monad

main :: IO ()
main = do
  [file] <- getArgs
  code <- readFile file
  hSetBuffering stdout NoBuffering
  interact $ runBrainfuckCode code 

class EOF a where
    eof :: a

instance EOF Integer where
    eof = -1

makeListWithEOF :: EOF a => [a] -> [a]
makeListWithEOF = (++ repeat eof)

runBrainfuckCode :: String -> String -> String
runBrainfuckCode source = map (chr . (max 0) . fromInteger) . runBrainfuck (parseSource source) . listToStream . makeListWithEOF . map (toInteger . ord)
