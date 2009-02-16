{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Parser.Token( braces
                                       , dot
                                       , identifier
                                       , natural
                                       , operator
                                       , parens
                                       , reserved
                                       , reservedOp
                                       , semi
                                       , symbol
                                       , whiteSpace
                                       )
where

import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token    as T
import           Text.ParserCombinators.Parsec.Language

functionalDef :: LanguageDef st
functionalDef = haskellStyle
                { reservedOpNames = ["\\", "->", "="]
                , reservedNames   = ["module", "where", "import"]
                }

functional :: T.TokenParser st
functional = T.makeTokenParser functionalDef

whiteSpace                      :: CharParser st ()
natural                         :: CharParser st Integer
dot, identifier, operator, semi :: CharParser st String
reserved, reservedOp            :: String                -> CharParser st ()
symbol                          :: String                -> CharParser st String
braces, parens                  :: CharParser st a       -> CharParser st a

braces     = T.braces     functional
dot        = T.dot        functional
identifier = T.identifier functional
natural    = T.natural    functional
operator   = T.operator   functional
parens     = T.parens     functional
reserved   = T.reserved   functional
reservedOp = T.reservedOp functional
semi       = T.semi       functional
symbol     = T.symbol     functional
whiteSpace = T.whiteSpace functional
