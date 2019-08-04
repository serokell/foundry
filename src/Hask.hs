{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (void)
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
    _ -> die "Usage: hask FILE.sd"
  runSource haskPlugin mParsedValue

haskPlugin :: Plugin
haskPlugin =
  Plugin
    { _pluginSchema = haskSchema,
      _pluginRecLayouts = haskRecLayouts
    }

haskSchema :: Schema
haskSchema =
  Schema
    { schemaTypes =
        [
          "Mod"  ==> tMod,
          "Var"  ==> tVar,
          "Str"  ==> tStr,
          "Lam"  ==> tLam,
          "App"  ==> tApp,
          "QVar" ==> tQVar,
          "Sig"  ==> tSig,
          "Bind" ==> tBind,
          "Data" ==> tData
        ]
    }
  where
    tVar = TyStr (void re) where
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
    tStr = TyStr (void (RE.many RE.anySym))
    tQVar =
      TyRec [
        "q" ==> uT "Var",
        "v" ==> uT "Var" <> uT "QVar"
      ]
    tMod =
      TyRec [
        "name" ==> uT "Var" <> uT "QVar",
        "ex"   ==> uS' (uT "Var"),
        "ds"   ==> uS' uDecl
      ]
    tLam =
      TyRec [
        "v" ==> uT "Var",
        "b" ==> uExpr
      ]
    tApp =
      TyRec [
        "f" ==> uExpr,
        "a" ==> uExpr
      ]
    tSig =
      TyRec [
        "v" ==> uT "Var",
        "t" ==> uExpr
      ]
    tBind =
      TyRec [
        "v" ==> uT "Var",
        "b" ==> uExpr
      ]
    tData =
      TyRec [
        "v"    ==> uT "Var",
        "alts" ==> uS uExpr
      ]
    uExpr =
      mconcat [
        uT "Lam",
        uT "App",
        uT "Str",
        uT "Var",
        uT "QVar"
      ]
    uDecl =
      mconcat [
        uT "Sig",
        uT "Bind",
        uT "Data"
      ]

haskRecLayouts :: HashMap TyName ALayoutFn
haskRecLayouts = recLayouts
  where
    recLayouts =
      [
        "Lam"  ==> recLayoutLam,
        "App"  ==> recLayoutApp,
        "Mod"  ==> recLayoutMod,
        "QVar" ==> recLayoutQVar,
        "Sig"  ==> recLayoutSig,
        "Bind" ==> recLayoutBind,
        "Data" ==> recLayoutData
      ]
    recLayoutQVar =
      field "q" noPrec <> "." <> field "v" precAllowAll
    recLayoutApp =
      field "f" (precAllow ["App"]) <>
      field "a" (precAllow ["Var", "QVar"])
    recLayoutLam =
      jumptag "λ" <> field "v" precAllowAll
      `vsep` field "b" precAllowAll
    recLayoutMod =
      jumptag "module" <> field "name" (precAllow ["Var", "QVar"]) <> field "ex" precAllowAll
      `vsep` field "ds" precAllowAll
    recLayoutSig =
      field "v" noPrec <> jumptag "::" <> field "t" precAllowAll
    recLayoutBind =
      field "v" noPrec <> jumptag "=" <> field "b" precAllowAll
    recLayoutData =
      jumptag "data" <> field "v" noPrec <> "=" <> field "alts" precAllowAll
