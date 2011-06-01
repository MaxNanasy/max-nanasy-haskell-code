module Language.Lisp.Symbol where

import Language.Lisp.Monad

import Control.Monad.State

import Data.List

getInternTable :: Lisp [Object]
getInternTable = liftInternTable get

putInternTable :: [Object] -> Lisp ()
putInternTable = liftInternTable . put

hasName :: String -> Object -> Bool
hasName name (Symbol (Idd name' _)) = name == name'

intern :: String -> Lisp Object
intern name = do
  internTable <- getInternTable
  case find (hasName name) internTable of
    Just symbol -> return symbol
    Nothing     -> do
                n <- incrementIdCounter
                let symbol = Symbol (Idd name n)
                putInternTable $ symbol : internTable
                return symbol

symbolName :: Object -> String
symbolName (Symbol (Idd name _)) = name
symbolName _                     = error "symbol-name: Not a symbol."
