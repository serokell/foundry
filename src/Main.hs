module Main where

import Source
import Foundry.Syn (SynTop)

main :: IO ()
main = do
  _ :: SynTop <- runGUI
  return ()
