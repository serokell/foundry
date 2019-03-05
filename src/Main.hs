{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
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
  runGUI foundryPlugin (foundryInitEditorState et)

foundryPlugin :: Plugin
foundryPlugin =
  Plugin
    { _pluginTyEnv = foundryTyEnv,
      _pluginRecLayouts = foundryRecLayouts,
      _pluginNodeFactory = foundryNodeFactory
    }

foundryTyEnv :: Env
foundryTyEnv =
  Env
    { envMap =
        Map.fromList
          [ ("Nat", TyStr),
            ("Var", TyStr),
            ("IVar", tyIVar),
            ("Lam", tyLam),
            ("Pi", tyPi),
            ("App", tyApp),
            ("Star", TyRec []),
            ("Box", TyRec []) ]
    }
  where
    tyIVar =
      TyRec
        [ ("var", mkTyUnion ["Var"]),
          ("index", mkTyUnion ["Nat"]) ]
    tyLam =
      TyRec
        [ ("var", mkTyUnion ["Var"]),
          ("ty", tyExpr),
          ("body", tyExpr) ]
    tyPi =
      TyRec
        [ ("var", mkTyUnion ["Var"]),
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

foundryRecLayouts :: Map TyId RecLayoutFn
foundryRecLayouts = Map.fromList
  [ (mkTyId "Lam", recLayoutLam),
    (mkTyId "Pi", recLayoutPi),
    (mkTyId "App", field "fn" <+> field "arg"),
    (mkTyId "Star", "★"),
    (mkTyId "Box", "□"),
    (mkTyId "IVar", field "var" <+> "@" <+> field "index") ]
  where
    recLayoutLam =
      "λ" <+> field "var" <+> ":" <+> field "ty"
      -/-
      field "body"
    recLayoutPi =
      "Π" <+> field "var" <+> ":" <+> field "ty"
      -/-
      field "body"

foundryNodeFactory :: [NodeCreateFn]
foundryNodeFactory =
  [ NodeCreateFn insertModeEvent (mkTyId "Var"),
    NodeCreateFn insertModeEvent (mkTyId "Nat"),
    NodeCreateFn (shiftChar 'V') (mkTyId "IVar"),
    NodeCreateFn (shiftChar 'L') (mkTyId "Lam"),
    NodeCreateFn (shiftChar 'P') (mkTyId "Pi"),
    NodeCreateFn (shiftChar 'A') (mkTyId "App"),
    NodeCreateFn (shiftChar 'S') (mkTyId "Star"),
    NodeCreateFn (shiftChar 'B') (mkTyId "Box") ]

foundryInitEditorState :: String -> IO EditorState
foundryInitEditorState et = do
  expr <- synImportExpr <$>
    case M.P.exprFromText (fromString et) of
      Left err -> die (show err)
      Right e -> M.I.load Nothing e
  return $ EditorState expr offsetZero False False [] []

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
    Object tyId (ValueRec (SynRec (Map.fromList fields') sel))
  where
    tyId = mkTyId tyName
    fields' =
      [ (mkFieldId tyName fieldName, obj) |
        (fieldName, obj) <- fields ]
    fieldIds = map fst fields'
    sel = case index of
      Nothing -> RecSelSelf SelfSelEmpty
      Just i -> RecSelSelf (SelfSelChild (fieldIds !! i))

mkStrObject :: TyName -> Text -> Object
mkStrObject tyName str =
  Object
    (mkTyId tyName)
    (ValueStr (SynStr str (Text.length str) False))
