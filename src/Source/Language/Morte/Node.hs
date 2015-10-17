{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Source.Language.Morte.Node where

import Control.Lens hiding (Const, (%~))
import Data.Singletons
import Data.Singletons.TH
import Data.Text (Text)
import Data.Type.Equality
import qualified Data.Text.Lazy as Text.Lazy
import qualified Morte.Core as M

singletons [d|

  data LabelProduct = Lam | Pi | App
    deriving (Eq)
  data LabelSum = Expr
    deriving (Eq)
  data LabelEnd = Const | Var | Embed | Arg
    deriving (Eq)

  data Label
    = LabelProduct LabelProduct
    | LabelSum LabelSum
    | LabelEnd LabelEnd
    deriving (Eq)

  data RelationProduct
    = LamArg
    | LamExpr1
    | LamExpr2
    | PiArg
    | PiExpr1
    | PiExpr2
    | AppExpr1
    | AppExpr2
    deriving (Eq)

  data RelationSum
    = ExprConst
    | ExprVar
    | ExprLam
    | ExprPi
    | ExprApp
    | ExprEmbed
    deriving (Eq)

  |]

singletons [d|

  relationProductLabel :: RelationProduct -> LabelProduct
  relationProductLabel = \case
    LamArg   -> Lam
    LamExpr1 -> Lam
    LamExpr2 -> Lam
    PiArg    -> Pi
    PiExpr1  -> Pi
    PiExpr2  -> Pi
    AppExpr1 -> App
    AppExpr2 -> App

  relationProductColabel :: RelationProduct -> Label
  relationProductColabel = \case
    LamArg   -> LabelEnd Arg
    LamExpr1 -> LabelSum Expr
    LamExpr2 -> LabelSum Expr
    PiArg    -> LabelEnd Arg
    PiExpr1  -> LabelSum Expr
    PiExpr2  -> LabelSum Expr
    AppExpr1 -> LabelSum Expr
    AppExpr2 -> LabelSum Expr

  relationSumLabel :: RelationSum -> LabelSum
  relationSumLabel = \case
    ExprConst -> Expr
    ExprVar   -> Expr
    ExprLam   -> Expr
    ExprPi    -> Expr
    ExprApp   -> Expr
    ExprEmbed -> Expr

  relationSumColabel :: RelationSum -> Label
  relationSumColabel = \case
    ExprConst -> LabelEnd Const
    ExprVar   -> LabelEnd Var
    ExprLam   -> LabelProduct Lam
    ExprPi    -> LabelProduct Pi
    ExprApp   -> LabelProduct App
    ExprEmbed -> LabelEnd Embed

  |]

type family Repr (p :: LabelEnd) :: * where
  Repr 'Const = M.Const
  Repr 'Var   = M.Var
  Repr 'Arg   = Text

type FieldGetter p
   = forall r
   . RelationProductLabel r ~ p
  => Sing r
  -> Node (RelationProductColabel r)

-- This is a performance hack. In an ideal world `FieldGetter` would be sufficient.
data Fields (p :: LabelProduct) where
  FieldOverride
    :: RelationProductLabel r ~ p
    => Sing r
    -> Node (RelationProductColabel r)
    -> Fields p
    -> Fields p
  Fields :: FieldGetter p -> Fields p

fields :: Fields p -> FieldGetter p
fields = \case
  Fields f -> f
  FieldOverride r n get -> \r' -> case r %~ r' of
    Proved Refl -> n
    Disproved _ -> fields get r'

override
  :: forall r
   . SRelationProduct r
  -> (Node (RelationProductColabel r) -> Node (RelationProductColabel r))
  -> Node ('LabelProduct (RelationProductLabel r))
  -> Node ('LabelProduct (RelationProductLabel r))
override r f (s :- get) = s :- FieldOverride r (fields get r & f) (clean get)
  where
    clean :: Fields (RelationProductLabel r) -> Fields (RelationProductLabel r)
    clean fs = case fs of
      Fields{} -> fs
      FieldOverride r' n get' -> case r %~ r' of
        -- Assuming there's at most one FieldOverride for each possible
        -- relation, we do not need to clean further.
        Proved Refl -> get'
        Disproved _ -> FieldOverride r' n (clean get')

data Node (p :: Label) where
  (:-) :: Sing p -> Fields p -> Node ('LabelProduct p)
  (:>)
    :: SRelationSum r
    -> Node (RelationSumColabel r)
    -> Node ('LabelSum (RelationSumLabel r))
  End :: SingI p => Repr p -> Node ('LabelEnd p)

withProduct :: (SingI p => FieldGetter p -> r) -> Node ('LabelProduct p) -> r
withProduct f (s :- get) = withSingI s (f (fields get))

infix 9 :>
infix 9 :-

nodeImportArg :: Text.Lazy.Text -> NodeArg
nodeImportArg t = End (Text.Lazy.toStrict t)

mkProduct :: SingI p => FieldGetter p -> Node ('LabelProduct p)
mkProduct f = sing :- Fields f

mkLam :: NodeArg -> NodeExpr -> NodeExpr -> NodeLam
mkLam arg expr1 expr2 = mkProduct (\case
  SLamArg   -> arg
  SLamExpr1 -> expr1
  SLamExpr2 -> expr2
  _ -> error "Impossible happened")

mkPi :: NodeArg -> NodeExpr -> NodeExpr -> NodePi
mkPi arg expr1 expr2 = mkProduct (\case
  SPiArg   -> arg
  SPiExpr1 -> expr1
  SPiExpr2 -> expr2
  _ -> error "Impossible happened")

mkApp :: NodeExpr -> NodeExpr -> NodeApp
mkApp expr1 expr2 = mkProduct (\case
  SAppExpr1 -> expr1
  SAppExpr2 -> expr2
  _ -> error "Impossible happened")

onExpr
  :: (NodeConst -> r)
  -> (NodeVar -> r)
  -> (NodeLam -> r)
  -> (NodePi -> r)
  -> (NodeApp -> r)
  -> (NodeEmbed -> r)
  -> (NodeExpr -> r)
onExpr onConst onVar onLam onPi onApp onEmbed (s :> node) =
  (case s of
    SExprConst -> onConst
    SExprVar   -> onVar
    SExprLam   -> onLam
    SExprPi    -> onPi
    SExprApp   -> onApp
    SExprEmbed -> onEmbed
  ) node

nodeImportExpr :: M.Expr M.X -> NodeExpr
nodeImportExpr = \case
  M.Const c -> SExprConst :> End c
  M.Var   v -> SExprVar :> End v
  M.Lam x _A  b -> SExprLam :> mkLam (nodeImportArg x) (nodeImportExpr _A) (nodeImportExpr  b)
  M.Pi  x _A _B -> SExprPi  :> mkPi  (nodeImportArg x) (nodeImportExpr _A) (nodeImportExpr _B)
  M.App f a -> SExprApp :> mkApp (nodeImportExpr f) (nodeImportExpr a)
  M.Embed a -> M.absurd a

nodeExportArg :: NodeArg -> Text.Lazy.Text
nodeExportArg (End t) = Text.Lazy.fromStrict t

nodeExportExpr :: NodeExpr -> M.Expr M.X
nodeExportExpr = onExpr onConst onVar onLam onPi onApp onEmbed
  where
    onConst :: NodeConst -> M.Expr M.X
    onConst (End c) = M.Const c

    onVar :: NodeVar -> M.Expr M.X
    onVar (End v) = M.Var v

    onLam :: NodeLam -> M.Expr M.X
    onLam (_ :- get) = M.Lam x _A b
      where
        x  = nodeExportArg  (fields get SLamArg)
        _A = nodeExportExpr (fields get SLamExpr1)
        b  = nodeExportExpr (fields get SLamExpr2)

    onPi :: NodePi -> M.Expr M.X
    onPi (_ :- get) = M.Pi x _A _B
      where
        x  = nodeExportArg  (fields get SPiArg)
        _A = nodeExportExpr (fields get SPiExpr1)
        _B = nodeExportExpr (fields get SPiExpr2)

    onApp :: NodeApp -> M.Expr M.X
    onApp (_ :- get) = M.App f a
      where
        f = nodeExportExpr (fields get SAppExpr1)
        a = nodeExportExpr (fields get SAppExpr2)

    onEmbed :: NodeEmbed -> M.Expr M.X
    onEmbed (End r) = case r of {}

noded :: Iso' (M.Expr M.X) NodeExpr
noded = iso nodeImportExpr nodeExportExpr

type NodeLam = Node ('LabelProduct 'Lam)
type NodePi  = Node ('LabelProduct 'Pi)
type NodeApp = Node ('LabelProduct 'App)
type NodeExpr = Node ('LabelSum 'Expr)
type NodeConst = Node ('LabelEnd 'Const)
type NodeVar = Node ('LabelEnd 'Var)
type NodeEmbed = Node ('LabelEnd 'Embed)
type NodeArg = Node ('LabelEnd 'Arg)

data Path (p :: Label) (q :: Label) where
  Here :: SingI p => Path p p
  (:@-)
    :: SRelationProduct r
    -> Path (RelationProductColabel r) q
    -> Path ('LabelProduct (RelationProductLabel r)) q
  (:@>)
    :: SRelationSum r
    -> Path (RelationSumColabel r) q
    -> Path ('LabelSum (RelationSumLabel r)) q

instance Eq (Path p q) where
  Here        == Here        = True
  (r1 :@- p1) == (r2 :@- p2) | Proved Refl <- r1 %~ r2 = p1 == p2
  (r1 :@> p1) == (r2 :@> p2) | Proved Refl <- r1 %~ r2 = p1 == p2
  _           == _           = False

instance TestEquality (Path p) where
  testEquality p1 p2 = testEquality (pathTarget p1) (pathTarget p2)

type PathExpr = Path ('LabelSum 'Expr)

infixr 9 :@-
infixr 9 :@>

atPath :: Path p q -> Traversal' (Node p) (Node q)
atPath path h e = case path of
  Here -> h e
  r :@- p -> case e of
    _ :- get -> (\a -> override r (const a) e) <$> atPath p h (fields get r)
    _ -> pure e
  r :@> p -> case e of
    r' :> sub -> case r %~ r' of
      Proved Refl -> (r':>) <$> atPath p h sub
      Disproved _ -> pure e
    _ -> pure e

pathTarget :: Path p q -> Sing q
pathTarget = \case
  Here -> sing
  _ :@- p -> pathTarget p
  _ :@> p -> pathTarget p

pathSource :: Path p q -> Sing p
pathSource = \case
  Here -> sing
  r :@- _ -> SLabelProduct (sRelationProductLabel r)
  r :@> _ -> SLabelSum     (sRelationSumLabel     r)

data Discard t where
  Discard :: t a -> Discard t

instance Eq (Discard (Path p)) where
  Discard p1 == Discard p2
    | Just Refl <- testEquality p1 p2
    = p1 == p2
  _ == _ = False

-- testing definition
path0 :: Path ('LabelSum 'Expr) ('LabelEnd 'Const)
path0 = SExprLam :@> SLamExpr1 :@- SExprConst :@> Here

-- testing definition
expr0 :: NodeExpr
expr0 = SExprLam :> mkLam (End "star") (SExprConst :> End M.Star) (SExprConst :> End M.Star)
