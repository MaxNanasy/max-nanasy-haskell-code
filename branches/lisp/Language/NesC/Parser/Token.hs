module Language.NesC.Parser.Token
    ( braces
    , commaSep
    , dot
    , identifier
    , integer
    , parens
    , reserved
    , reservedOp
    , semi
    , sharpComments
    ) where

import qualified Text.ParserCombinators.Parsec.Token    as T
import           Text.ParserCombinators.Parsec.Language

nesCDef = emptyDef
          { commentStart = "/*"
          , commentEnd   = "*/"
          , commentLine  = "//"
          , reservedNames = ["as", "components", "configuration", "implementation", "new"]
          , reservedOpNames = ["->", "<-"]
          }

sharpCommentDef = emptyDef
                  { commentLine = "#" }

nesC = T.makeTokenParser nesCDef

braces     = T.braces     nesC
commaSep   = T.commaSep   nesC
dot        = T.dot        nesC
identifier = T.identifier nesC
integer    = T.integer    nesC
parens     = T.parens      nesC
reserved   = T.reserved   nesC
reservedOp = T.reservedOp nesC
semi       = T.semi       nesC

sharpComments = T.whiteSpace $ T.makeTokenParser sharpCommentDef
