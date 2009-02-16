module Language.Pascalish.Parser.Token
    ( colon
    , commaSep
    , identifier
    , integer
    , reserved
    , reservedOp
    , semi
    )
    where

import qualified Text.ParserCombinators.Parsec.Token    as T
import           Text.ParserCombinators.Parsec.Language

pascalishDef = emptyDef
               { commentLine     = "//"
               , reservedNames   = ["anytype", "begin", "call", "char", "end", "integer", "procedure", "program", "read", "short", "var", "write"]
               , reservedOpNames = ["=", "+", "-", "*", "/"]
               }

pascalish = T.makeTokenParser pascalishDef

colon      = T.colon      pascalish
commaSep   = T.commaSep   pascalish
identifier = T.identifier pascalish
integer    = T.integer    pascalish
reserved   = T.reserved   pascalish
reservedOp = T.reservedOp pascalish
semi       = T.semi       pascalish
