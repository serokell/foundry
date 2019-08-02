{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import System.Exit (die)
import System.Environment (getArgs)
import Text.Megaparsec as Megaparsec

import Sdam.Parser (pValue, parse)
import Source.NewGen
import Source

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
  runSource foundryPlugin mParsedValue

foundryPlugin :: Plugin
foundryPlugin =
  Plugin
    { _pluginSchema = foundrySchema,
      _pluginRecLayouts = foundryRecLayouts
    }

foundrySchema :: Schema
foundrySchema =
  Schema
    { schemaTypes =
        HashMap.fromList
          [ ("Nat", TyStr),
            ("Var", TyStr),
            ("IVar", tyIVar),
            ("Lam", tyLam),
            ("Pi", tyPi),
            ("App", tyApp),
            ("Star", TyRec []),
            ("Box", TyRec []) ]
    }
  where
    tyIVar =
      TyRec
        [ ("var", mkTyUnion ["Var"]),
          ("index", mkTyUnion ["Nat"]) ]
    tyLam =
      TyRec
        [ ("var", mkTyUnion ["Var"]),
          ("ty", tyExpr),
          ("body", tyExpr) ]
    tyPi =
      TyRec
        [ ("var", mkTyUnion ["Var"]),
          ("ty", tyExpr),
          ("body", tyExpr) ]
    tyApp =
      TyRec
        [ ("fn", tyExpr),
          ("arg", tyExpr) ]
    tyExpr =
      mkTyUnion
        [ "Lam",
          "Pi",
          "App",
          "Star",
          "Box",
          "Var",
          "IVar" ]

foundryRecLayouts :: HashMap TyName ALayoutFn
foundryRecLayouts = recLayouts
  where
    recLayouts = HashMap.fromList
      [ ("Lam", recLayoutLam),
        ("Pi", recLayoutPi),
        ("App", recLayoutApp),
        ("Star", jumptag "★"),
        ("Box", jumptag "□"),
        ("IVar", recLayoutIVar) ]
    precAll = precAllow (HashMap.keysSet recLayouts)
    precAtoms = ["Star", "Box"]
    prec ss = precAllow (ss <> precAtoms)
    recLayoutApp =
     field "fn" (prec ["App"]) <>
     field "arg" (prec [])
    recLayoutLam =
      jumptag "λ" <> field "var" noPrec <> ":" <> field "ty" precAll
      `vsep` field "body" precAll
    recLayoutPi =
      jumptag "Π" <> field "var" noPrec <> ":" <> field "ty" precAll
      `vsep` field "body" precAll
    recLayoutIVar =
      field "var" noPrec <> jumptag "@" <> field "index" noPrec
