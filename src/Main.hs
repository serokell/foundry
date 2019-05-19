{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Void
import System.Exit (die)
import System.Environment (getArgs)
import Data.String (fromString)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

import Source.NewGen
import Source

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

main :: IO ()
main = do
  et <- getArgs >>= \case
    [et] -> return et
    _ -> die "Usage: foundry EXPR"
  tyEnv <- either die return =<< readTyEnv "morte.sdam"
  runGUI (foundryPlugin tyEnv) (foundryInitEditorState et)

foundryPlugin :: Env -> Plugin
foundryPlugin env =
  Plugin
    { _pluginTyEnv = env,
      _pluginRecLayouts = foundryRecLayouts,
      _pluginNodeFactory = foundryNodeFactory
    }

foundryRecLayouts :: HashMap TyName ALayoutFn
foundryRecLayouts = recLayouts
  where
    recLayouts = HashMap.fromList
      [ ("Lam", recLayoutLam),
        ("Pi", recLayoutPi),
        ("App", recLayoutApp),
        ("Star", "★"),
        ("Box", "□"),
        ("IVar", recLayoutIVar) ]
    precAll = precAllow (HashMap.keysSet recLayouts)
    precAtoms = ["Star", "Box"]
    prec ss = precAllow (ss <> precAtoms)
    recLayoutApp =
     field "fn" (prec ["App"]) <>
     field "arg" (prec [])
    recLayoutLam =
      "λ" <> field "var" noPrec <> ":" <> field "ty" precAll
      `vsep` field "body" precAll
    recLayoutPi =
      "Π" <> field "var" noPrec <> ":" <> field "ty" precAll
      `vsep` field "body" precAll
    recLayoutIVar =
      field "var" noPrec <> "@" <> field "index" noPrec

foundryNodeFactory :: [NodeCreateFn]
foundryNodeFactory =
  [ NodeCreateFn insertModeEvent "Var",
    NodeCreateFn insertModeEvent "Nat",
    NodeCreateFn (shiftChar 'V') "IVar",
    NodeCreateFn (shiftChar 'L') "Lam",
    NodeCreateFn (shiftChar 'P') "Pi",
    NodeCreateFn (shiftChar 'A') "App",
    NodeCreateFn (shiftChar 'S') "Star",
    NodeCreateFn (shiftChar 'B') "Box" ]

foundryInitEditorState :: String -> IO EditorState
foundryInitEditorState et = do
  expr <- synImportExpr <$>
    case M.P.exprFromText (fromString et) of
      Left err -> die (show err)
      Right e -> M.I.load Nothing e
  return $ EditorState expr offsetZero False False WritingDirectionLTR [] []

synImportExpr :: M.Expr Void -> Holey Object
synImportExpr = Solid . \case
  M.Const c -> synImportConst c
  M.Var v -> synImportVar v
  M.Lam x _A b -> synImportLam x _A b
  M.Pi  x _A _B -> synImportPi x _A _B
  M.App f a -> synImportApp f a
  M.Embed e -> absurd e

synImportConst :: M.Const -> Object
synImportConst = \case
  M.Star -> mkRecObject "Star" [] Nothing
  M.Box -> mkRecObject "Box" [] Nothing

synImportVar :: M.Var -> Object
synImportVar = \case
  M.V t 0 -> mkStrObject "Var" (Text.Lazy.toStrict t)
  M.V t n ->
    mkRecObject "IVar"
      [ ("var", Solid $ mkStrObject "Var" (Text.Lazy.toStrict t)),
        ("index", Solid $ mkStrObject "Nat" (Text.pack (show n))) ]
      (Just 0)

synImportLam :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> Object
synImportLam x _A b =
  mkRecObject "Lam"
    [ ("var", Solid $ mkStrObject "Var" (Text.Lazy.toStrict x)),
      ("ty", synImportExpr _A),
      ("body", synImportExpr b) ]
    (Just 2)

synImportPi :: Text.Lazy.Text -> M.Expr Void -> M.Expr Void -> Object
synImportPi x _A _B =
  mkRecObject "Pi"
    [ ("var", Solid $ mkStrObject "Var" (Text.Lazy.toStrict x)),
      ("ty", synImportExpr _A),
      ("body", synImportExpr _B) ]
    (Just 2)

synImportApp :: M.Expr Void -> M.Expr Void -> Object
synImportApp f a =
  mkRecObject "App"
    [ ("fn", synImportExpr f),
      ("arg", synImportExpr a) ]
    (Just 0)

mkRecObject :: TyName -> [(FieldName, Holey Object)] -> Maybe Int -> Object
mkRecObject tyName fields index =
    Object tyName (ValueRec (SynRec (HashMap.fromList fields) sel))
  where
    sel = case index of
      Nothing -> RecSelSelf SelfSelEmpty
      Just i -> RecSelSelf (SelfSelChild (fst (fields !! i)))

mkStrObject :: TyName -> Text -> Object
mkStrObject tyName str =
  Object tyName (ValueStr (SynStr str (Text.length str) False))
