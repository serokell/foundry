module Main where

import Source
import Source.Language.Morte as Morte

main :: IO ()
main = do
    _ :: Morte.State <- runGUI
    return ()
