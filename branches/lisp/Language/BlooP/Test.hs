{-# OPTIONS_GHC -Wall -fno-implicit-prelude #-}

module Language.BlooP.Test where

import Language.BlooP

twoToTheThreeToThe :: Function
twoToTheThreeToThe n = do
  cell 0 <-- 1
  loop n $ do
    cell 0 <-- 3 * cell 0
  output <-- 1
  loop (cell 0) $ do
    output <-- 2 * output
