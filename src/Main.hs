module Main where

import Source
import Foundry.Syn (TOP)
import Foundry.Syn.Common (SYN)

main :: IO ()
main = do
    _ :: SYN (TOP Int Int) <- runGUI
    return ()
