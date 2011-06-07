module Main (main) where

import Language.Lisp

import System.Environment
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    []     -> run stdin stdin stdout stderr
    [file] -> runFile file
    _      -> error $ "Expected [0, 1] arguments; received " ++ (show $ length args) ++ "."

runFile :: FilePath -> IO ()
runFile file = do
  replStream <- openFile file ReadMode
  run replStream stdin stdout stderr
