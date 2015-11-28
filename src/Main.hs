module Main where

import Source
import Source.Language.Morte.Syn as Morte

main :: IO ()
main = do
    _ :: Morte.Syn Int Int <- runGUI
    return ()
