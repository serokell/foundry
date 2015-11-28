module Main where

import Source
import Foundry.Syn as Morte

main :: IO ()
main = do
    _ :: Morte.Syn Int Int <- runGUI
    return ()
