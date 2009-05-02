{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Language.Brainfuck.Parser(parseSource, ProgramSource) where

import Data.Maybe
import Data.Monoid

import Text.ParserCombinators.Parsec

import Language.Brainfuck.Class

type AList k v = [(k, v)]

instructionAL ::MonoidBF m => AList Char m
instructionAL = [('+', incCell    ),
                 ('-', decCell    ),
                 ('>', incPointer ),
                 ('<', decPointer ),
                 ('.', outputCell ),
                 (',', inputCell  )]

instance MonoidBF ProgramSource where
    incCell       = "+"
    decCell       = "-"
    incPointer    = ">"
    decPointer    = "<"
    outputCell    = "."
    inputCell     = ","
    loopUntilZero = ('[':) . (++ "]")

type ProgramSource = String

type ProgramParser     m = GenParser Char () [m]
type InstructionParser m = GenParser Char () m

parseSource :: MonoidBF m => ProgramSource -> m
parseSource = either
              (error . show)
              mconcat
              . parse source "Brainfuck Source"

source :: MonoidBF m => ProgramParser m
source = do
  program <- many instruction
  eof
  return program

instruction :: MonoidBF m => InstructionParser m
instruction = loop <|> simpleInstruction <|> comment

loop :: MonoidBF m => InstructionParser m
loop = return . loopUntilZero . mconcat =<< between (char '[') (char ']') loopBody

loopBody :: MonoidBF m => ProgramParser m
loopBody = many instruction

simpleInstruction :: MonoidBF m => InstructionParser m
simpleInstruction = do
  c <- oneOf "+-><.,"
  return $ fromJust $ lookup c instructionAL

comment :: MonoidBF m => InstructionParser m
comment = noneOf "]" >> return mempty
