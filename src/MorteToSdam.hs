{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Void
import qualified Morte.Core as M
import qualified Morte.Import as M.I
import qualified Morte.Parser as M.P
import Sdam.Core
import Sdam.Printer
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  et <- getArgs >>= \case
    [et] -> return et
    _ -> die "Usage: morte-to-sdam EXPR"
  expr <-
    case M.P.exprFromText (fromString et) of
      Left err -> die (show err)
      Right e -> M.I.load Nothing e
  putStrLn $ render (rValue (convertExpr expr))

convertExpr :: M.Expr Void -> RenderValue
convertExpr = \case
  M.Const c -> convertConst c
  M.Var v -> convertVar v
  M.Lam x _A b -> convertLam x _A b
  M.Pi x _A _B -> convertPi x _A _B
  M.App f a -> convertApp f a
  M.Embed e -> absurd e

convertConst :: M.Const -> RenderValue
convertConst = \case
  M.Star -> mkRecValue "star" []
  M.Box -> mkRecValue "box" []

convertVar :: M.Var -> RenderValue
convertVar = \case
  M.V t 0 -> mkStrValue "v" (Text.Lazy.toStrict t)
  M.V t n ->
    mkRecValue
      "iv"
      [ ("var", mkStrValue "v" (Text.Lazy.toStrict t)),
        ("index", mkStrValue "nat" (Text.pack (show n)))
      ]

convertLam :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> RenderValue
convertLam x _A b =
  mkRecValue
    "lam"
    [ ("var", mkStrValue "v" (Text.Lazy.toStrict x)),
      ("ty", convertExpr _A),
      ("body", convertExpr b)
    ]

convertPi :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> RenderValue
convertPi x _A _B =
  mkRecValue
    "pi"
    [ ("var", mkStrValue "v" (Text.Lazy.toStrict x)),
      ("ty", convertExpr _A),
      ("body", convertExpr _B)
    ]

convertApp :: M.Expr Void -> M.Expr Void -> RenderValue
convertApp f a =
  mkRecValue
    "a"
    [ ("fn", convertExpr f),
      ("arg", convertExpr a)
    ]

mkRecValue :: TyName -> [(FieldName, RenderValue)] -> RenderValue
mkRecValue tyName fields =
  RenderValue (ValueRec tyName (HashMap.fromList fields))

mkStrValue :: TyName -> Text -> RenderValue
mkStrValue tyName str =
  RenderValue (ValueStr tyName str)
