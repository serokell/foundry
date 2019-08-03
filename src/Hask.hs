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
        HashMap.fromList
          [ ("Mod", tyMod),
            ("Var", tyVar),
            ("Str", tyStr),
            ("Lam", tyLam),
            ("App", tyApp),
            ("QVar", tyQVar),
            ("Sig", tySig),
            ("Bind", tyBind)
          ]
    }
  where
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
    tyStr = TyStr (void (RE.many RE.anySym))
    tyQVar =
      TyRec
        [ ("q", mkTyUnion ["Var"] Nothing),
          ("v", mkTyUnion ["Var", "QVar"] Nothing) ]
    tyMod =
      TyRec
        [ ("name", mkTyUnion ["Var", "QVar"] Nothing),
          ("ex", mkTyUnion [] (Just (mkTyUnion ["Var"] Nothing))),
          ("ds", mkTyUnion [] (Just tyDecl)) ]
    tyLam =
      TyRec
        [ ("v", mkTyUnion ["Var"] Nothing),
          ("b", tyExpr) ]
    tyApp =
      TyRec
        [ ("f", tyExpr),
          ("a", tyExpr) ]
    tySig =
      TyRec
        [ ("v", mkTyUnion ["Var"] Nothing),
          ("t", tyExpr) ]
    tyBind =
      TyRec
        [ ("v", mkTyUnion ["Var"] Nothing),
          ("b", tyExpr) ]
    tyExpr =
      mkTyUnion
        [ "Lam",
          "App",
          "Str",
          "Var",
          "QVar" ]
        Nothing
    tyDecl =
      mkTyUnion
        [ "Sig",
          "Bind" ]
        Nothing

haskRecLayouts :: HashMap TyName ALayoutFn
haskRecLayouts = recLayouts
  where
    recLayouts = HashMap.fromList
      [ ("Lam", recLayoutLam),
        ("App", recLayoutApp),
        ("Mod", recLayoutMod),
        ("QVar", recLayoutQVar),
        ("Sig", recLayoutSig),
        ("Bind", recLayoutBind)
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
