module Foundry.Syn where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Function
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.Dynamic

import Data.Vinyl

import Source.Syntax
import Source.Draw
import Source.Input

import Foundry.Syn.Text
import Foundry.Syn.Hole
import Foundry.Syn.Sum
import Foundry.Syn.Expr
import Foundry.Syn.Const
import Foundry.Syn.Arg
import Foundry.Syn.Var
import Foundry.Syn.Record
import Foundry.Syn.Common

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

synImportText :: Text.Lazy.Text -> SynText
synImportText t =
  let t' = Text.Lazy.toStrict t
  in SynText t' (Text.length t') False

synImportConst :: M.Const -> SynConst
synImportConst = \case
  M.Star -> SynConstStar
  M.Box -> SynConstBox

synImportVar :: M.Var -> SynVar
synImportVar (M.V t n) = SynVar (synImportText t) n

synImportArg :: Text.Lazy.Text -> SynArg
synImportArg t = SynArg (synImportText t)

synImportLam :: Text.Lazy.Text -> M.Expr M.X -> M.Expr M.X -> SynLam
synImportLam x _A  b = SynRecord
   ( SynRecField (synImportArg x)
  :& SynRecField (SynSolid (synImportExpr _A))
  :& SynRecField (SynSolid (synImportExpr b))
  :& RNil )
  2
  True

synImportPi :: Text.Lazy.Text -> M.Expr M.X -> M.Expr M.X -> SynPi
synImportPi  x _A _B = SynRecord
   ( SynRecField (synImportArg x)
  :& SynRecField (SynSolid (synImportExpr _A))
  :& SynRecField (SynSolid (synImportExpr _B))
  :& RNil )
  2
  True

synImportApp :: M.Expr M.X -> M.Expr M.X -> SynApp
synImportApp f a = SynRecord
   ( SynRecField (SynSolid (synImportExpr f))
  :& SynRecField (SynSolid (synImportExpr a))
  :& RNil )
  0
  True

synImportExpr :: M.Expr M.X -> SynExpr
synImportExpr = \case
  {- TODO: autolift '[SynLam, SynPi, SynApp, SynConst, SynVar, SynEmbed] -}
  M.Const c -> SynAddend . SynAddend . SynAddend . SynAugend $ synImportConst c
  M.Var v -> SynAddend . SynAddend . SynAddend . SynAddend . SynAugend $ synImportVar v
  M.Lam x _A b -> SynAugend $ synImportLam x _A b
  M.Pi  x _A _B -> SynAddend . SynAugend $ synImportPi x _A _B
  M.App f a -> SynAddend . SynAddend . SynAugend $ synImportApp f a
  M.Embed e -> M.absurd e

data SynTop = SynTop
  { _synExpr            :: SynHole SynExpr
  , _synPointer         :: Offset
  , _synHoverBarEnabled :: Bool
  , _synUndo            :: [SynHole SynExpr]
  , _synRedo            :: [SynHole SynExpr]
  }

makeLenses ''SynTop

instance SyntaxLayout ActiveZone Viewport SynTop where
  layout syn = reader $ \viewport ->
    let
      hoverBar = do
        guard $ syn ^. synHoverBarEnabled
        [text . Text.pack . show $ syn ^. synPointer]
      bars = concat [hoverBar]
      lctx = LayoutCtx True Seq.empty
    in flip (<>) (vertical bars)
     . hover (outline light1) (syn ^. synPointer)
     . background dark1
     . center (viewport ^. _Viewport)
     . sel (lctx & lctxSelected &&~ synSelfSelected (syn ^. synExpr))
     . pad (LRTB 5 5 5 5)
     $ runReader (layout (syn ^. synExpr)) lctx

instance SyntaxReact () ActiveZone SynTop where
  react = asum handlers
    where
      handlers =
        [ handlePointerMotion
        -- , handleButtonPress
        , handleCtrl_h
        , handleRedirectExpr
        , handle_u
        , handleCtrl_r ]
      handlePointerMotion = do
        PointerMotion x y <- view rctxInputEvent
        synPointer .= Offset (fromIntegral x) (fromIntegral y)
      -- FIXME: this function
      -- handleButtonPress = do
      --   ButtonPress <- view rctxInputEvent
      --   pointer <- use synPointer
      --   Just (_, _, p) <- activate pointer <$> view rctxLastLayout
      --   zoom synExpr $ modify (updateExprPath p)
      handleCtrl_h = do
        KeyPress [Control] keyCode <- view rctxInputEvent
        guard $ keyLetter 'h' keyCode
        synHoverBarEnabled %= not
      handle_u = do
        KeyPress [] keyCode <- view rctxInputEvent
        guard $ keyLetter 'u' keyCode
        (u:us) <- use synUndo
        expr <- use synExpr
        synRedo %= (expr:)
        synUndo .= us
        synExpr .= u
      handleCtrl_r = do
        KeyPress [Control] keyCode <- view rctxInputEvent
        guard $ keyLetter 'r' keyCode
        (r:rs) <- use synRedo
        expr <- use synExpr
        synUndo %= (expr:)
        synRedo .= rs
        synExpr .= r
      handleRedirectExpr = do
        expr <- use synExpr
        reactRedirect synExpr
        expr' <- use synExpr
        unless (undoEq expr expr') $ do
          synUndo %= (expr:)
          synRedo .= []

updateExprPath :: Path -> SynHole SynExpr -> SynHole SynExpr
updateExprPath path
  = over _SynSolid
  $ over _SynAugend (updateLamPath path)
  . over (_SynAddend . _SynAugend) (updatePiPath  path)
  . over (_SynAddend . _SynAddend . _SynAugend) (updateAppPath path)

updateLamPath :: Path -> SynLam -> SynLam
updateLamPath path e =
  case uncons path of
    Nothing -> e & synSelectionSelf .~ True
    Just (fromDynamic -> Just sel', sels) -> e
      & synSelectionSelf .~ False
      & synSelection .~ sel'
      & case sel' of
          SelLamExpr1 -> synField SSelLamExpr1 %~ updateExprPath sels
          SelLamExpr2 -> synField SSelLamExpr2 %~ updateExprPath sels
          SelLamArg   -> id
    _ -> e

updatePiPath :: Path -> SynPi -> SynPi
updatePiPath path e =
  case uncons path of
    Nothing -> e & synSelectionSelf .~ True
    Just (fromDynamic -> Just sel', sels) -> e
      & synSelectionSelf .~ False
      & synSelection .~ sel'
      & case sel' of
          SelPiExpr1 -> synField SSelPiExpr1 %~ updateExprPath sels
          SelPiExpr2 -> synField SSelPiExpr2 %~ updateExprPath sels
          SelPiArg   -> id
    _ -> e

updateAppPath :: Path -> SynApp -> SynApp
updateAppPath path e =
  case uncons path of
    Nothing -> e & synSelectionSelf .~ True
    Just (fromDynamic -> Just sel', sels) -> e
      & synSelectionSelf .~ False
      & synSelection .~ sel'
      & case sel' of
          SelAppExpr1 -> synField SSelAppExpr1 %~ updateExprPath sels
          SelAppExpr2 -> synField SSelAppExpr2 %~ updateExprPath sels
    _ -> e

instance SyntaxBlank SynTop where
  blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    expr <- synImportExpr <$> case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> M.I.load Nothing e
    return $ SynTop (SynSolid expr) offsetZero False [] []
