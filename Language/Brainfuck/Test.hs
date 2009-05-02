module Language.Brainfuck.Test where

import Language.Brainfuck
import Language.Brainfuck.Parser

import Data.Stream

import Data.Char

class EOF a where
    eof :: a

instance EOF Integer where
    eof = -1

makeListWithEOF :: EOF a => [a] -> [a]
makeListWithEOF = (++ repeat eof)

test :: [Char] -> [Char] -> [Char]
test source = map (chr . fromInteger) . runBrainfuck (parseSource source) . listToStream . makeListWithEOF . map (toInteger . ord)
