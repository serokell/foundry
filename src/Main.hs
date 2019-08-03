{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (void)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import System.Exit (die)
import System.Environment (getArgs)
import Text.Megaparsec as Megaparsec
import Text.Regex.Applicative as RE
import Data.Char as Char
import Data.List as List

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
          [ ("Nat", tyNat),
            ("Var", tyVar),
            ("IVar", tyIVar),
            ("Lam", tyLam),
            ("Pi", tyPi),
            ("App", tyApp),
            ("Star", TyRec []),
            ("Box", TyRec []) ]
    }
  where
    tyNat = TyStr (void re)
      where
        re = RE.some (RE.psym Char.isDigit)
    tyVar = TyStr (void re)
      where
        re = re_alphavar <|> re_op
        re_fst =
          RE.psym $ \c ->
            Char.isLetter c ||
            c == '_'
        re_labelchar =
          RE.psym $ \c ->
            Char.isLetter c ||
            Char.isDigit c ||
            c == '_'
        re_opchar =
          RE.psym $ \c ->
            c `List.elem` ("!#$%&*+./<=>?@^|-~" :: [Char])
        re_alphavar =
          re_fst *> RE.many re_labelchar
        re_op =
          RE.some re_opchar
    tyIVar =
      TyRec
        [ ("var", mkTyUnion ["Var"] Nothing),
          ("index", mkTyUnion ["Nat"] Nothing) ]
    tyLam =
      TyRec
        [ ("var", mkTyUnion ["Var"] Nothing),
          ("ty", tyExpr),
          ("body", tyExpr) ]
    tyPi =
      TyRec
        [ ("var", mkTyUnion ["Var"] Nothing),
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
        Nothing

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
