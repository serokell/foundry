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
          "all"        ==> TyDefnRec [],
          "module"     ==> TyDefnRec ["name", "ex", "ds"],
          "v"          ==> TyDefnStr,
          "str"        ==> TyDefnStr,
          "lam"        ==> TyDefnRec ["v", "b"],
          "a"          ==> TyDefnRec ["f", "a"],
          "qv"         ==> TyDefnRec ["q", "v"],
          "sig"        ==> TyDefnRec ["v", "t"],
          "as-pat"     ==> TyDefnRec ["alias", "p"],
          "bind"       ==> TyDefnRec ["v", "b"],
          "data"       ==> TyDefnRec ["v", "alts"],
          "import"     ==> TyDefnRec ["module", "e"],
          "qualified"  ==> TyDefnRec ["entities"],
          "as-mod"     ==> TyDefnRec ["module", "alias"]
        ],
      schemaRoot = tMod
    }
  where
    tVar =
        uT "v" $
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
    tStr =
      uT "str" $
      TyInstStr (void (RE.many RE.anySym))
    tQVar =
      uT "qv" $
      TyInstRec [
        "q" ==> tVar,
        "v" ==> tVar <> tQVar
      ]
    tAll =
      uT "all" $
      TyInstRec []
    tMod =
      uT "module" $
      TyInstRec [
        "name" ==> tVar <> tQVar,
        "ex"   ==> tAll <> uS' tVar,
        "ds"   ==> uS' tDecl
      ]
    tLam =
      uT "lam" $
      TyInstRec [
        "v" ==> tVar,
        "b" ==> tExpr
      ]
    tExprApp =
      uT "a" $
      TyInstRec [
        "f" ==> tExpr,
        "a" ==> tExpr
      ]
    tPatApp =
      uT "a" $
      TyInstRec [
        "f" ==> tPat,
        "a" ==> tPat
      ]
    tTypeApp =
      uT "a" $
      TyInstRec [
        "f" ==> tType,
        "a" ==> tType
      ]
    tDeclSig =
      uT "sig" $
      TyInstRec [
        "v" ==> tVar <> uS tVar,
        "t" ==> tType
      ]
    tExprSig =
      uT "sig" $
      TyInstRec [
        "v" ==> tExpr,
        "t" ==> tType
      ]
    tPatSig =
      uT "sig" $
      TyInstRec [
        "v" ==> tPat,
        "t" ==> tType
      ]
    tTypeSig =
      uT "sig" $
      TyInstRec [
        "v" ==> tType,
        "t" ==> tKind
      ]
    tBind =
      uT "bind" $
      TyInstRec [
        "v" ==> tPat,
        "b" ==> tExpr
      ]
    tData =
      uT "data" $
      TyInstRec [
        "v"    ==> tVar,
        "alts" ==> uS tExpr
      ]
    tImport =
      uT "import" $
      TyInstRec [
        "module" ==> tVar <> tQVar <> tAsMod,
        "e" ==> tAll <> uS' tVar <> tQualified
      ]
    tAsMod =
      uT "as-mod" $
      TyInstRec [
        "module" ==> tVar <> tQVar,
        "alias" ==> tVar <> tQVar
      ]
    tQualified =
      uT "qualified" $
      TyInstRec [
        "entities" ==> tAll <> uS' tVar
      ]
    tAsPat =
      uT "as-pat" $
      TyInstRec [
        "alias" ==> tVar,
        "p" ==> tPat
      ]
    tExpr =
      mconcat [
        tLam,
        tExprApp,
        tStr,
        tVar,
        tQVar,
        tExprSig
      ]
    tKind = tType
    tType =
      mconcat [
        tVar,
        tQVar,
        tTypeApp,
        tTypeSig
        -- tForall
      ]
    tPat =
      mconcat [
        tVar,
        tQVar,
        tPatApp,
        tPatSig,
        tAsPat
      ]
    tDecl =
      mconcat [
        tDeclSig,
        tBind,
        tData,
        tImport
      ]

haskRecLayouts :: HashMap TyName ALayoutFn
haskRecLayouts = recLayouts
  where
    recLayouts =
      [
        "all"         ==> recLayoutAll,
        "lam"         ==> recLayoutLam,
        "a"           ==> recLayoutApp,
        "module"      ==> recLayoutMod,
        "qv"          ==> recLayoutQVar,
        "sig"         ==> recLayoutSig,
        "as-pat"      ==> recLayoutAsPat,
        "bind"        ==> recLayoutBind,
        "data"        ==> recLayoutData,
        "import"      ==> recLayoutImport,
        "qualified"   ==> recLayoutQualified,
        "as-mod"      ==> recLayoutAsMod
      ]
    recLayoutAll = jumptag "∗"
    recLayoutQVar =
      field "q" noPrec "q" <> "." <> field "v" precAllowAll "v"
    recLayoutApp =
      field "f" (precAllow ["a"]) "function" <>
      field "a" (precAllow ["v", "qv"]) "argument"
    recLayoutLam =
      jumptag "λ" <> field "v" precAllowAll "variable"
      `vsep` field "b" precAllowAll "body"
    recLayoutMod =
      jumptag "module" <> field "name" (precAllow ["v", "qv"]) "name" <> "exports" <> field "ex" precAllowAll "entities"
      `vsep` field "ds" precAllowAll "declarations"
    recLayoutSig =
      field "v" noPrec "variable" <> jumptag "::" <> field "t" precAllowAll "type"
    recLayoutAsPat =
      field "alias" noPrec "alias" <> jumptag "@" <> field "p" noPrec "pattern"
    recLayoutBind =
      field "v" noPrec "variable" <> jumptag "=" <> field "b" precAllowAll "body"
    recLayoutData =
      jumptag "data" <> field "v" noPrec "name" <> "=" <> field "alts" precAllowAll "alternatives"
    recLayoutImport =
      jumptag "from" <> field "module" (precAllow ["v", "qv", "as-mod"]) "module" <> "import" <> field "e" precAllowAll "entities"
    recLayoutQualified =
      jumptag "qualified" <> field "entities" (precAllow ["v"]) "entities"
    recLayoutAsMod =
      field "module" noPrec "module" <> jumptag "as" <> field "alias" noPrec "alias"
