module Foundry.Syn where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Function
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.List.NonEmpty (nonEmpty)

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
synImportLam x _A  b =
  SynRecord
    { _synRec =
        synImportArg x :&
        SynSolid (synImportExpr _A) :&
        SynSolid (synImportExpr b) :&
        RNil,
      _synRecSel = IS (IS IZ),
      _synRecSelSelf = True }

synImportPi :: Text.Lazy.Text -> M.Expr M.X -> M.Expr M.X -> SynPi
synImportPi  x _A _B =
  SynRecord
    { _synRec =
        synImportArg x :&
        SynSolid (synImportExpr _A) :&
        SynSolid (synImportExpr _B) :&
        RNil,
      _synRecSel = IS (IS IZ),
      _synRecSelSelf = True }

synImportApp :: M.Expr M.X -> M.Expr M.X -> SynApp
synImportApp f a =
  SynRecord
    { _synRec =
        SynSolid (synImportExpr f) :&
        SynSolid (synImportExpr a) :&
        RNil,
      _synRecSel = IZ,
      _synRecSelSelf = True }

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

instance SyntaxLayout Path Viewport SynTop where
  layout syn = reader $ \viewport ->
    let
      hoverBar = do
        guard $ syn ^. synHoverBarEnabled
        [text . Text.pack . show $ syn ^. synPointer]
      bars = concat @[] [hoverBar]
      withBars =
        case nonEmpty bars of
          Nothing -> id
          Just bars' -> \c ->
            collageCompose offsetZero c (vertical bars')
      centered c =
        let
          padding = centerOf (viewport ^. _Viewport) c
          backgroundRect = rect nothing (inj dark1)
        in
          substrate padding backgroundRect c
      lctx = LayoutCtx True Seq.empty
    in withBars
     . centered
     . layoutSel (lctx & lctxSelected &&~ synSelfSelected (syn ^. synExpr))
     . pad (LRTB 5 5 5 5)
     $ runReader (layout (syn ^. synExpr)) lctx

instance SyntaxReact () Path SynTop where
  react = asum @[] handlers
    where
      handlers =
        [ handlePointerMotion
        , handleButtonPress
        , handleCtrl_h
        , handleRedirectExpr
        , handle_u
        , handleCtrl_r ]
      handlePointerMotion = do
        PointerMotion x y <- view rctxInputEvent
        synPointer .= Offset (fromIntegral x) (fromIntegral y)
      handleButtonPress = do
        ButtonPress <- view rctxInputEvent
        pointer <- use synPointer
        Just (_, _, p) <-
          activate pointer . collageElements offsetZero <$>
            view rctxLastLayout
        zoom synExpr $ modify (updateExprPath p)
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
    Just (fromPathSegment -> Just idx, sels) ->
      e & synSelectionSelf .~ False
        & synSelection .~ idx
        & case idx :: Idx (Fields LabelLam) of
            IZ -> id
            IS IZ -> synField @(NN 1) %~ updateExprPath sels
            IS (IS IZ) -> synField @(NN 2) %~ updateExprPath sels
            IS (IS (IS i)) -> case i of {}
    _ -> e

updatePiPath :: Path -> SynPi -> SynPi
updatePiPath path e =
  case uncons path of
    Nothing -> e & synSelectionSelf .~ True
    Just (fromPathSegment -> Just idx, sels) ->
      e & synSelectionSelf .~ False
        & synSelection .~ idx
        & case idx :: Idx (Fields LabelPi) of
            IZ -> id
            IS IZ -> synField @(NN 1) %~ updateExprPath sels
            IS (IS IZ) -> synField @(NN 2) %~ updateExprPath sels
            IS (IS (IS i)) -> case i of {}
    _ -> e

updateAppPath :: Path -> SynApp -> SynApp
updateAppPath path e =
  case uncons path of
    Nothing -> e & synSelectionSelf .~ True
    Just (fromPathSegment -> Just idx, sels) ->
      e & synSelectionSelf .~ False
        & synSelection .~ idx
        & case idx :: Idx (Fields LabelApp) of
            IZ -> synField @(NN 0) %~ updateExprPath sels
            IS IZ -> synField @(NN 1) %~ updateExprPath sels
            IS (IS i) -> case i of {}
    _ -> e

instance SyntaxBlank SynTop where
  blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    expr <- synImportExpr <$> case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> M.I.load Nothing e
    return $ SynTop (SynSolid expr) offsetZero False [] []
