{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import Data.Void
import System.Exit (die)
import System.Environment (getArgs)
import Data.String (fromString)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

import Sdam.Core
import Sdam.Printer

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

main :: IO ()
main = do
  et <- getArgs >>= \case
    [et] -> return et
    _ -> die "Usage: morte-to-sdam EXPR"
  expr <-
    case M.P.exprFromText (fromString et) of
      Left err -> die (show err)
      Right e -> M.I.load Nothing e
  putStrLn $ render (rObject (convertExpr expr))

convertExpr :: M.Expr Void -> RenderObject
convertExpr = \case
  M.Const c -> convertConst c
  M.Var v -> convertVar v
  M.Lam x _A b -> convertLam x _A b
  M.Pi  x _A _B -> convertPi x _A _B
  M.App f a -> convertApp f a
  M.Embed e -> absurd e

convertConst :: M.Const -> RenderObject
convertConst = \case
  M.Star -> mkRecObject "Star" []
  M.Box -> mkRecObject "Box" []

convertVar :: M.Var -> RenderObject
convertVar = \case
  M.V t 0 -> mkStrObject "Var" (Text.Lazy.toStrict t)
  M.V t n ->
    mkRecObject "IVar"
      [ ("var", mkStrObject "Var" (Text.Lazy.toStrict t)),
        ("index", mkStrObject "Nat" (Text.pack (show n))) ]

convertLam :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> RenderObject
convertLam x _A b =
  mkRecObject "Lam"
    [ ("var", mkStrObject "Var" (Text.Lazy.toStrict x)),
      ("ty", convertExpr _A),
      ("body", convertExpr b) ]

convertPi :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> RenderObject
convertPi x _A _B =
  mkRecObject "Pi"
    [ ("var", mkStrObject "Var" (Text.Lazy.toStrict x)),
      ("ty", convertExpr _A),
      ("body", convertExpr _B) ]

convertApp :: M.Expr Void -> M.Expr Void -> RenderObject
convertApp f a =
  mkRecObject "App"
    [ ("fn", convertExpr f),
      ("arg", convertExpr a) ]

mkRecObject :: TyName -> [(FieldName, RenderObject)] -> RenderObject
mkRecObject tyName' fields =
  RenderObject (Object tyName' (ValueRec (HashMap.fromList fields)))

mkStrObject :: TyName -> Text -> RenderObject
mkStrObject tyName' str =
  RenderObject (Object tyName' (ValueStr str))
