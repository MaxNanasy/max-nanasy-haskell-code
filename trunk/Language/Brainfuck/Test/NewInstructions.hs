{-# OPTIONS_GHC -Wall #-}

module Language.Brainfuck.Test.NewInstructions where

import Language.Brainfuck.Class
import Language.Brainfuck.Parser

ps :: MonoidBF m => ProgramSource -> m
ps = parseSource

zeroCell :: MonoidBF m => m
zeroCell = ps "[-]"
