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
        [
          "Nat"  ==> TyDefnStr,
          "Var"  ==> TyDefnStr,
          "IVar" ==> TyDefnRec ["var", "index"],
          "Lam"  ==> TyDefnRec ["var", "ty", "body"],
          "Pi"   ==> TyDefnRec ["var", "ty", "body"],
          "App"  ==> TyDefnRec ["fn", "arg"],
          "Star" ==> TyDefnRec [],
          "Box"  ==> TyDefnRec []
        ],
      schemaRoot = tExpr
    }
  where
    tNat =
        uT "Nat" $
        TyInstStr (void re)
      where
        re = RE.some (RE.psym Char.isDigit)
    tVar =
        uT "Var" $
        TyInstStr (void re)
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
    tIVar =
      uT "IVar" $
      TyInstRec [
        "var"   ==> tVar,
        "index" ==> tNat
      ]
    tLam =
      uT "Lam" $
      TyInstRec [
        "var"  ==> tVar,
        "ty"   ==> tExpr,
        "body" ==> tExpr
      ]
    tPi =
      uT "Pi" $
      TyInstRec [
        "var"  ==> tVar,
        "ty"   ==> tExpr,
        "body" ==> tExpr
      ]
    tApp =
      uT "App" $
      TyInstRec [
        "fn"  ==> tExpr,
        "arg" ==> tExpr
      ]
    tStar =
      uT "Star" $
      TyInstRec []
    tBox =
      uT "Box" $
      TyInstRec []
    tExpr =
      mconcat [
        tLam,
        tPi,
        tApp,
        tStar,
        tBox,
        tVar,
        tIVar
      ]

foundryRecLayouts :: HashMap TyName ALayoutFn
foundryRecLayouts = recLayouts
  where
    recLayouts =
      [
        "Lam"  ==> recLayoutLam,
        "Pi"   ==> recLayoutPi,
        "App"  ==> recLayoutApp,
        "Star" ==> jumptag "★",
        "Box"  ==> jumptag "□",
        "IVar" ==> recLayoutIVar
      ]
    precAll = precAllow (HashMap.keysSet recLayouts)
    precAtoms = ["Star", "Box"]
    prec ss = precAllow (ss <> precAtoms)
    recLayoutApp =
     field "fn" (prec ["App"]) "function" <>
     field "arg" (prec []) "argument"
    recLayoutLam =
      jumptag "λ" <> field "var" noPrec "variable" <> ":" <> field "ty" precAll "type"
      `vsep` field "body" precAll "body"
    recLayoutPi =
      jumptag "Π" <> field "var" noPrec "variable" <> ":" <> field "ty" precAll "type"
      `vsep` field "body" precAll "body"
    recLayoutIVar =
      field "var" noPrec "variable" <> jumptag "@" <> field "index" noPrec "index"
