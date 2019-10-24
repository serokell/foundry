{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List as List
import Sdam.Parser (pValue, parse)
import Source
import Source.NewGen
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec as Megaparsec
import Text.Regex.Applicative as RE

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
        [ "nat" ==> TyDefnStr,
          "v" ==> TyDefnStr, -- variable
          "iv" ==> TyDefnRec ["var", "index"], -- indexed variable
          "lam" ==> TyDefnRec ["var", "ty", "body"],
          "pi" ==> TyDefnRec ["var", "ty", "body"],
          "a" ==> TyDefnRec ["fn", "arg"], -- function application
          "star" ==> TyDefnRec [],
          "box" ==> TyDefnRec []
        ],
      schemaRoot = tExpr
    }
  where
    tNat =
      uT "nat" $
        TyInstStr (void re)
      where
        re = RE.some (RE.psym Char.isDigit)
    tVar =
      uT "v" $
        TyInstStr (void re)
      where
        re = re_alphavar <|> re_op
        re_fst =
          RE.psym $ \c ->
            Char.isLetter c
              || c == '_'
        re_labelchar =
          RE.psym $ \c ->
            Char.isLetter c
              || Char.isDigit c
              || c == '_'
        re_opchar =
          RE.psym $ \c ->
            c `List.elem` ("!#$%&*+./<=>?@^|-~" :: [Char])
        re_alphavar =
          re_fst *> RE.many re_labelchar
        re_op =
          RE.some re_opchar
    tIVar =
      uT "iv" $
        TyInstRec
          [ "var" ==> tVar,
            "index" ==> tNat
          ]
    tLam =
      uT "lam" $
        TyInstRec
          [ "var" ==> tVar,
            "ty" ==> tExpr,
            "body" ==> tExpr
          ]
    tPi =
      uT "pi" $
        TyInstRec
          [ "var" ==> tVar,
            "ty" ==> tExpr,
            "body" ==> tExpr
          ]
    tApp =
      uT "a" $
        TyInstRec
          [ "fn" ==> tExpr,
            "arg" ==> tExpr
          ]
    tStar =
      uT "star" $
        TyInstRec []
    tBox =
      uT "box" $
        TyInstRec []
    tExpr =
      mconcat
        [ tLam,
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
      [ "lam" ==> recLayoutLam,
        "pi" ==> recLayoutPi,
        "a" ==> recLayoutApp,
        "star" ==> jumptag "★",
        "box" ==> jumptag "□",
        "iv" ==> recLayoutIVar
      ]
    precAll = precAllow (HashMap.keysSet recLayouts)
    precAtoms = ["star", "box"]
    prec ss = precAllow (ss <> precAtoms)
    recLayoutApp =
      field "fn" (prec ["a"]) "function"
        <> field "arg" (prec []) "argument"
    recLayoutLam =
      jumptag "λ" <> field "var" noPrec "variable" <> ":" <> field "ty" precAll "type"
        `vsep` field "body" precAll "body"
    recLayoutPi =
      jumptag "Π" <> field "var" noPrec "variable" <> ":" <> field "ty" precAll "type"
        `vsep` field "body" precAll "body"
    recLayoutIVar =
      field "var" noPrec "variable" <> jumptag "@" <> field "index" noPrec "index"
