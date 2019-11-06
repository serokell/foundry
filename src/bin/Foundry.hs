{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sdam.Parser (pValue, parse)
import Source
import Source.Language.Morte
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec as Megaparsec

main :: IO ()
main = do
  mParsedValue <- getArgs >>= \case
    [filepath] -> do
      content <- readFile filepath
      case parse pValue filepath content of
        Left e -> die (Megaparsec.errorBundlePretty e)
        Right a -> return (Just a)
    [] -> return Nothing
    _ -> die "Usage: foundry FILE.sd"
  runSource mortePlugin mParsedValue
