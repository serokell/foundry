{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Source.Language.Morte.Node where

import Control.Lens hiding (Const)
import qualified Data.Node as N
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Map as Map
import qualified Morte.Core as M

data Label
  = Lam
  | Pi
  | App
  | Const
  | Var
  | Embed
  | Arg
  deriving (Eq, Ord, Show)

data Relation
  = LamArg
  | LamExpr1
  | LamExpr2
  | PiArg
  | PiExpr1
  | PiExpr2
  | AppExpr1
  | AppExpr2
  deriving (Eq, Ord, Show)

data Representation
  = RepConst M.Const
  | RepVar   M.Var
  | RepArg   Text

type Node = N.Node Representation Relation Label
type Path = N.Path Relation

exprLabels :: [Label]
exprLabels = [Lam, Pi, App, Const, Var, Embed]

slaveLabels :: Relation -> [Label]
slaveLabels = \case
  LamArg   -> [Arg]
  LamExpr1 -> exprLabels
  LamExpr2 -> exprLabels
  PiArg    -> [Arg]
  PiExpr1  -> exprLabels
  PiExpr2  -> exprLabels
  AppExpr1 -> exprLabels
  AppExpr2 -> exprLabels

nodeLabel :: Traversal' Node Label
nodeLabel = N.nodeLabel

_Path :: Iso' Path [Relation]
_Path = N._Path

atPath :: Path -> Traversal' Node Node
atPath = N.atPath

rp :: Relation -> Path
rp r = N.Path [r]

mkHole :: () -> Node
mkHole = N.Hole

mkLam :: Node -> Node -> Node -> Node
mkLam arg expr1 expr2
  = N.Node Lam
  $ [ (LamArg, arg)
    , (LamExpr1, expr1)
    , (LamExpr2, expr2)
    ] & Map.fromList

mkPi :: Node -> Node -> Node -> Node
mkPi arg expr1 expr2
  = N.Node Pi
  $ [ (PiArg, arg)
    , (PiExpr1, expr1)
    , (PiExpr2, expr2)
    ] & Map.fromList

mkApp :: Node -> Node -> Node
mkApp expr1 expr2
  = N.Node App
  $ [ (AppExpr1, expr1)
    , (AppExpr2, expr2)
    ] & Map.fromList

onConst
  :: (()      -> r)
  -> (M.Const -> r)
  -> (Node    -> r)
onConst onHole f = \case
  N.Hole h -> onHole h
  N.Rep Const (RepConst c) -> f c
  _ -> error "onConst: not a Const"

onVar
  :: (()    -> r)
  -> (M.Var -> r)
  -> (Node  -> r)
onVar onHole f = \case
  N.Hole h -> onHole h
  N.Rep Var (RepVar v) -> f v
  _ -> error "onVar: not a Var"

onArg
  :: (()   -> r)
  -> (Text -> r)
  -> (Node -> r)
onArg onHole f = \case
  N.Hole h -> onHole h
  N.Rep Arg (RepArg a) -> f a
  _ -> error "onArg: not an Arg"

onLam :: (Node -> Node -> Node -> r) -> Node -> r
onLam f = \case
  N.Node Lam m -> f (m Map.! LamArg) (m Map.! LamExpr1) (m Map.! LamExpr2)
  _ -> error "onLam: not a Lam"

onPi :: (Node -> Node -> Node -> r) -> Node -> r
onPi f = \case
  N.Node Pi m -> f (m Map.! PiArg) (m Map.! PiExpr1) (m Map.! PiExpr2)
  _ -> error "onPi: not a Pi"

onApp :: (Node -> Node -> r) -> Node -> r
onApp f = \case
  N.Node App m -> f (m Map.! AppExpr1) (m Map.! AppExpr2)
  _ -> error "onApp: not an App"

onExpr
  :: (()   -> r)
  -> (Node -> r)
  -> (Node -> r)
  -> (Node -> r)
  -> (Node -> r)
  -> (Node -> r)
  -> (Node -> r)
  -> (Node -> r)
onExpr onHole onConst onVar onLam onPi onApp onEmbed node =
  case node ^? N.nodeLabel of
    Just Const -> onConst node
    Just Var   -> onVar   node
    Just Lam   -> onLam   node
    Just Pi    -> onPi    node
    Just App   -> onApp   node
    Just Embed -> onEmbed node
    Nothing    -> maybe (error "onHole: impossible") onHole (node ^? N.nodeHole)
    _          -> error "onExpr: not an Expr"

nodeImportArg :: Text.Lazy.Text -> Node
nodeImportArg t = (N.Rep Arg . RepArg) (Text.Lazy.toStrict t)

nodeImportExpr :: M.Expr M.X -> Node
nodeImportExpr = \case
  M.Const c -> (N.Rep Const . RepConst) c
  M.Var   v -> (N.Rep Var . RepVar) v
  M.Lam x _A  b -> mkLam (nodeImportArg x) (nodeImportExpr _A) (nodeImportExpr  b)
  M.Pi  x _A _B -> mkPi  (nodeImportArg x) (nodeImportExpr _A) (nodeImportExpr _B)
  M.App f a -> mkApp (nodeImportExpr f) (nodeImportExpr a)
  M.Embed a -> M.absurd a
