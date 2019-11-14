{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Sequence as Seq
import Data.String (fromString)
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
  M.Star -> mkRecValue "★" []
  M.Box -> mkRecValue "□" []

convertStr :: Text.Lazy.Text -> SynShape
convertStr = Syn . Seq.fromList . map TokenChar . Text.Lazy.unpack

convertVar :: M.Var -> RenderValue
convertVar = \case
  M.V t 0 -> mkRecValue (convertStr t) []
  M.V t n ->
    mkRecValue
      "_@_"
      [ mkRecValue (convertStr t) [],
        mkRecValue (fromString (show n)) []
      ]

convertLam :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> RenderValue
convertLam x _A b =
  mkRecValue
    "λ_:_/_"
    [ mkRecValue (convertStr x) [],
      convertExpr _A,
      convertExpr b
    ]

convertPi :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> RenderValue
convertPi x _A _B =
  mkRecValue
    "Π_:_/_"
    [ mkRecValue (convertStr x) [],
      convertExpr _A,
      convertExpr _B
    ]

convertApp :: M.Expr Void -> M.Expr Void -> RenderValue
convertApp f a =
  mkRecValue
    "__"
    [ convertExpr f,
      convertExpr a
    ]

mkRecValue :: SynShape -> [RenderValue] -> RenderValue
mkRecValue shape fields = RenderValue (synReconstruct shape fields)
