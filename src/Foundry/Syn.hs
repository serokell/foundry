{-# OPTIONS -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Function
import Data.Void

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Source.Collage.Builder (horizontal, vertical, getExtents)
import Source.Syntax
import Source.Draw
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Text
import Foundry.Syn.Hole
import Foundry.Syn.Common

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

data SelLam = SelLamArg | SelLamExpr1 | SelLamExpr2
  deriving (Eq, Ord, Enum, Show)

data SelPi = SelPiArg | SelPiExpr1 | SelPiExpr2
  deriving (Eq, Ord, Enum, Show)

data SelApp = SelAppExpr1 | SelAppExpr2
  deriving (Eq, Ord, Enum, Show)

data SynLam = SynLam
  { _synLamArg   :: SynArg
  , _synLamExpr1 :: SynHole SynExpr
  , _synLamExpr2 :: SynHole SynExpr
  , _synLamSel   :: Maybe SelLam
  } deriving (Eq, Ord, Show)

data SynPi = SynPi
  { _synPiArg   :: SynArg
  , _synPiExpr1 :: SynHole SynExpr
  , _synPiExpr2 :: SynHole SynExpr
  , _synPiSel   :: Maybe SelPi
  } deriving (Eq, Ord, Show)

data SynApp = SynApp
  { _synAppExpr1 :: SynHole SynExpr
  , _synAppExpr2 :: SynHole SynExpr
  , _synAppSel   :: Maybe SelApp
  } deriving (Eq, Ord, Show)

data SynConst = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

data SynEmbed
  = SynEmbedFilePath SynText
  | SynEmbedURL SynText
  deriving (Eq, Ord, Show)

newtype SynArg = SynArg SynText
  deriving (Eq, Ord, Show)

data SynVar = SynVar
  { _synVarName  :: SynText
  , _synVarIndex :: Int
  } deriving (Eq, Ord, Show)

data SynExpr
  = SynExprLam   SynLam
  | SynExprPi    SynPi
  | SynExprApp   SynApp
  | SynExprConst SynConst
  | SynExprVar   SynVar
  | SynExprEmbed SynEmbed
  deriving (Eq, Ord, Show)

data SynTop n = SynTop
  { _synExpr            :: SynHole SynExpr
  , _synPointer         :: Offset n
  , _synHoverBarEnabled :: Bool
  , _synUndo            :: [SynHole SynExpr]
  , _synRedo            :: [SynHole SynExpr]
  } deriving (Eq, Show)

makeLenses ''SynTop
makeLenses ''SynVar
makePrisms ''SynArg
makeLenses ''SynLam
makeLenses ''SynPi
makeLenses ''SynApp
makePrisms ''SynExpr

synImportExpr :: M.Expr M.X -> SynExpr
synImportExpr =
  let
    synImportText t =
      let t' = Text.Lazy.toStrict t
      in SynText t' (Text.length t') False
    synImportConst = \case
      M.Star -> SynConstStar
      M.Box  -> SynConstBox
    synImportVar (M.V t n) = SynVar (synImportText t) n
    synImportArg t = SynArg (synImportText t)
    synImportLam x _A  b = SynLam
      (synImportArg x)
      (SynSolid (synImportExpr _A))
      (SynSolid (synImportExpr b))
      Nothing
    synImportPi  x _A _B = SynPi
      (synImportArg x)
      (SynSolid (synImportExpr _A))
      (SynSolid (synImportExpr _B))
      Nothing
    synImportApp f a = SynApp
      (SynSolid (synImportExpr f))
      (SynSolid (synImportExpr a))
      Nothing
  in \case
    M.Const c -> SynExprConst $ synImportConst c
    M.Var   v -> SynExprVar   $ synImportVar   v
    M.Lam x _A  b -> SynExprLam $ synImportLam x _A  b
    M.Pi  x _A _B -> SynExprPi  $ synImportPi  x _A _B
    M.App f a -> SynExprApp $ synImportApp f a
    M.Embed e -> M.absurd e

instance SynSelection SynLam SelLam where
  synSelection = view synLamSel

instance SynSelection SynPi SelPi where
  synSelection = view synPiSel

instance SynSelection SynApp SelApp where
  synSelection = view synAppSel

instance SynSelection SynArg Void
instance SynSelection SynConst Void
instance SynSelection SynVar Void
instance SynSelection SynEmbed Void

instance SynSelection SynExpr () where
  synSelection = \case
    SynExprLam   a -> () <$ synSelection a
    SynExprPi    a -> () <$ synSelection a
    SynExprApp   a -> () <$ synSelection a
    SynExprConst a -> () <$ synSelection a
    SynExprVar   a -> () <$ synSelection a
    SynExprEmbed a -> () <$ synSelection a

---       Arg       ---
---    instances    ---

instance UndoEq SynArg where
  undoEq (SynArg s1) (SynArg s2) = undoEq s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynArg where
  layout (SynArg t) = layout t

instance n ~ Int => SyntaxReact n ActiveZone SynArg where
  react = asum handlers
    where
      handlers = [reactRedirect _SynArg, handle_x]
      handle_x = do
        guardInputEvent $ keyCodeLetter KeyCode.Delete 'x'
        _SynArg .= mempty


---       Lam       ---
---    instances    ---

instance UndoEq SynLam where
  undoEq s1 s2
     = on undoEq (view synLamArg)   s1 s2
    && on undoEq (view synLamExpr1) s1 s2
    && on undoEq (view synLamExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynLam where
  layout syn = reader $ \lctx ->
    let
      maxWidth = (max `on` view pointX . getExtents) header body
      header =
        [ extend (Point 4 0) (punct "λ")
        , [ selLayout
              lctx
              (SelLamArg, view synLamArg)
              (join pad (Point 4 0))
              syn
          , join pad (Point 4 0) (punct ":")
          , selLayout
              lctx
              (SelLamExpr1, view synLamExpr1)
              (join pad (Point 4 0))
              syn
          ] & horizontal
        ] & horizontal
      body = selLayout lctx (SelLamExpr2, view synLamExpr2) id syn
    in
      [ header
      , join pad (Point 0 4) (line light1 maxWidth)
      , body
      ] & vertical

instance n ~ Int => SyntaxReact n ActiveZone SynLam where
  react = asum handlers
    where
      handlers =
        [ handleSelRedirect
        , handleArrowLeft
        , handleArrowRight
        , handleArrowUp
        , handleArrowDown ]
      handleArrowUp = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
        Just _ <- gets synSelection
        synLamSel .= Nothing
      handleArrowDown = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
        Nothing <- gets synSelection
        synLamSel .= Just SelLamExpr2
      handleArrowLeft = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
        use synLamSel >>= \case
          Just SelLamExpr2 -> synLamSel . _Just .= SelLamExpr1
          Just SelLamExpr1 -> synLamSel . _Just .= SelLamArg
          _ -> mzero
      handleArrowRight = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
        use synLamSel >>= \case
          Just SelLamArg   -> synLamSel . _Just .= SelLamExpr1
          Just SelLamExpr1 -> synLamSel . _Just .= SelLamExpr2
          _ -> mzero
      handleSelRedirect = do
        Just selection <- gets synSelection
        case selection of
          SelLamArg   -> reactRedirect synLamArg
          SelLamExpr1 -> reactRedirect synLamExpr1
          SelLamExpr2 -> reactRedirect synLamExpr2
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'L' keyCode
    return SynLam
      { _synLamArg   = SynArg mempty
      , _synLamExpr1 = SynHollow
      , _synLamExpr2 = SynHollow
      , _synLamSel   = Just SelLamArg }

---        Pi       ---
---    instances    ---

instance UndoEq SynPi where
  undoEq s1 s2
     = on undoEq (view synPiArg)   s1 s2
    && on undoEq (view synPiExpr1) s1 s2
    && on undoEq (view synPiExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynPi where
  layout syn = reader $ \lctx ->
    let
      maxWidth = (max `on` view pointX . getExtents) header body
      header =
        [ extend (Point 4 0) (punct "Π")
        , [ selLayout
              lctx
              (SelPiArg, view synPiArg)
              (join pad (Point 4 0))
              syn
          , join pad (Point 4 0) (punct ":")
          , selLayout
              lctx
              (SelPiExpr1, view synPiExpr1)
              (join pad (Point 4 0))
              syn
          ] & horizontal
        ] & horizontal
      body = selLayout lctx (SelPiExpr2, view synPiExpr2) id syn
    in
      [ header
      , join pad (Point 0 4) (line light1 maxWidth)
      , body
      ] & vertical

instance n ~ Int => SyntaxReact n ActiveZone SynPi where
  react = asum handlers
    where
      handlers =
        [ handleSelRedirect
        , handleArrowLeft
        , handleArrowRight
        , handleArrowUp
        , handleArrowDown ]
      handleArrowUp = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
        Just _ <- gets synSelection
        synPiSel .= Nothing
      handleArrowDown = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
        Nothing <- gets synSelection
        synPiSel .= Just SelPiExpr2
      handleArrowLeft = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
        use synPiSel >>= \case
          Just SelPiExpr2 -> synPiSel . _Just .= SelPiExpr1
          Just SelPiExpr1 -> synPiSel . _Just .= SelPiArg
          _ -> mzero
      handleArrowRight = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
        use synPiSel >>= \case
          Just SelPiArg   -> synPiSel . _Just .= SelPiExpr1
          Just SelPiExpr1 -> synPiSel . _Just .= SelPiExpr2
          _ -> mzero
      handleSelRedirect = do
        Just selection <- gets synSelection
        case selection of
          SelPiArg   -> reactRedirect synPiArg
          SelPiExpr1 -> reactRedirect synPiExpr1
          SelPiExpr2 -> reactRedirect synPiExpr2
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'P' keyCode
    return SynPi
      { _synPiArg   = SynArg mempty
      , _synPiExpr1 = SynHollow
      , _synPiExpr2 = SynHollow
      , _synPiSel   = Just SelPiArg }


---       App       ---
---    instances    ---

instance UndoEq SynApp where
  undoEq s1 s2
     = on undoEq (view synAppExpr1) s1 s2
    && on undoEq (view synAppExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynApp where
  layout syn = reader $ \lctx ->
    [ selLayout lctx
        (SelAppExpr1, view synAppExpr1)
        (join pad (Point 5 5))
        syn
    , join pad (Point 5 5)
      $ selLayout lctx
          (SelAppExpr2, view synAppExpr2)
          (outline dark2 . join pad (Point 5 5))
          syn
    ] & horizontalCenter

instance n ~ Int => SyntaxReact n ActiveZone SynApp where
  react = asum handlers
    where
      handlers =
        [ handleSelRedirect
        , handleArrowLeft
        , handleArrowRight
        , handleArrowUp
        , handleArrowDown ]
      handleArrowUp = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
        Just _ <- gets synSelection
        synAppSel .= Nothing
      handleArrowDown = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
        Nothing <- gets synSelection
        synAppSel .= Just SelAppExpr1
      handleArrowLeft = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
        use synAppSel >>= \case
          Just SelAppExpr2 -> synAppSel . _Just .= SelAppExpr1
          _ -> mzero
      handleArrowRight = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
        use synAppSel >>= \case
          Just SelAppExpr1 -> synAppSel . _Just .= SelAppExpr2
          _ -> mzero
      handleSelRedirect = do
        Just selection <- gets synSelection
        case selection of
          SelAppExpr1 -> reactRedirect synAppExpr1
          SelAppExpr2 -> reactRedirect synAppExpr2
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'A' keyCode
    return SynApp
      { _synAppExpr1 = SynHollow
      , _synAppExpr2 = SynHollow
      , _synAppSel   = Just SelAppExpr1 }


---      Const      ---
---    instances    ---

instance UndoEq SynConst where
  undoEq = (==)

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynConst where
  layout = pure . \case
    SynConstStar -> punct "★"
    SynConstBox  -> punct "□"

instance n ~ Int => SyntaxReact n ActiveZone SynConst where
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    if | keyLetter 'S' keyCode -> return SynConstStar
       | keyLetter 'B' keyCode -> return SynConstBox
       | otherwise -> mzero

---       Var       ---
---    instances    ---

instance UndoEq SynVar where
  undoEq s1 s2
     = on undoEq (view synVarName)  s1 s2
    && on (==)   (view synVarIndex) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynVar where
  layout v = reader $ \lctx ->
    [ runReader (layout (v ^. synVarName)) lctx
    , if (v ^. synVarIndex) > 0
      then layoutIndex (v ^. synVarIndex)
      else
        mempty
    ] & horizontal
    where
      layoutIndex = text . Text.map toSub . Text.pack . show
      toSub :: Char -> Char
      toSub = \case
        '0' -> '₀'
        '1' -> '₁'
        '2' -> '₂'
        '3' -> '₃'
        '4' -> '₄'
        '5' -> '₅'
        '6' -> '₆'
        '7' -> '₇'
        '8' -> '₈'
        '9' -> '₉'
        c   -> c

instance n ~ Int => SyntaxReact n ActiveZone SynVar where
  react = asum handlers
    where
      handlers =
        [ handleShiftUp
        , handleShiftDown
        , reactRedirect synVarName ]
      handleShiftUp = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'U' keyCode
        synVarIndex += 1
      handleShiftDown = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'D' keyCode
        synVarIndex %= max 0 . subtract 1
  subreact = do
    KeyPress [] keyCode <- view rctxInputEvent
    guard $ keyLetter 'i' keyCode
    return $ SynVar mempty 0

---      Embed      ---
---    instances    ---

instance UndoEq SynEmbed where
  undoEq (SynEmbedFilePath t1) (SynEmbedFilePath t2) = undoEq t1 t2
  undoEq (SynEmbedURL      t1) (SynEmbedURL      t2) = undoEq t1 t2
  undoEq  _                     _                    = False

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynEmbed where
  layout _ = pure (text "Embed")

instance n ~ Int => SyntaxReact n ActiveZone SynEmbed where

---       Expr      ---
---    instances    ---

instance UndoEq SynExpr where
  undoEq (SynExprLam   a1) (SynExprLam   a2) = undoEq a1 a2
  undoEq (SynExprPi    a1) (SynExprPi    a2) = undoEq a1 a2
  undoEq (SynExprApp   a1) (SynExprApp   a2) = undoEq a1 a2
  undoEq (SynExprConst a1) (SynExprConst a2) = undoEq a1 a2
  undoEq (SynExprVar   a1) (SynExprVar   a2) = undoEq a1 a2
  undoEq (SynExprEmbed a1) (SynExprEmbed a2) = undoEq a1 a2
  undoEq  _                 _                = False

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynExpr where
  layout = \case
    SynExprLam   a -> layout a
    SynExprPi    a -> layout a
    SynExprApp   a -> layout a
    SynExprConst a -> layout a
    SynExprVar   a -> layout a
    SynExprEmbed a -> layout a

instance n ~ Int => SyntaxReact n ActiveZone SynExpr where
  react = asum
    [ reactRedirect _SynExprLam
    , reactRedirect _SynExprPi
    , reactRedirect _SynExprApp
    , reactRedirect _SynExprConst
    , reactRedirect _SynExprVar
    , reactRedirect _SynExprEmbed ]
  subreact = asum
    [ subreactRedirect _SynExprLam
    , subreactRedirect _SynExprPi
    , subreactRedirect _SynExprApp
    , subreactRedirect _SynExprConst
    , subreactRedirect _SynExprVar
    , subreactRedirect _SynExprEmbed ]


---       Top       ---
---    instances    ---

instance n ~ Int => SyntaxLayout n ActiveZone (Viewport n) (SynTop n) where
  layout syn = reader $ \viewport ->
    let
      hoverBar = do
        guard $ syn ^. synHoverBarEnabled
        [text . Text.pack . show $ syn ^. synPointer]
      bars = concat [hoverBar]
      lctx = LayoutCtx True Seq.empty
    in flip mappend (vertical bars)
     . hover (outline light1) (syn ^. synPointer)
     . background dark1
     . center (viewport ^. _Viewport)
     . sel (lctx & lctxSelected &&~ synSelectionSelf (syn ^. synExpr))
     . join pad (Point 5 5)
     $ runReader (layout (syn ^. synExpr)) lctx

instance n ~ Int => SyntaxReact n ActiveZone (SynTop n) where
  react = asum handlers
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
        synPointer .= Point x y
      handleButtonPress = do
        ButtonPress <- view rctxInputEvent
        pointer <- use synPointer
        Just el <- activate pointer <$> view rctxLastLayout
        zoom synExpr $ modify (updateExprPath (el ^. elementObject))
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
  $ over _SynExprLam (updateLamPath path)
  . over _SynExprPi  (updatePiPath  path)
  . over _SynExprApp (updateAppPath path)

updateLamPath :: Path -> SynLam -> SynLam
updateLamPath path e =
  case uncons path of
    Nothing -> e & synLamSel .~ Nothing
    Just (toEnum -> sel', sels) -> e
      & synLamSel .~ Just sel'
      & case sel' of
          SelLamExpr1 -> synLamExpr1 %~ updateExprPath sels
          SelLamExpr2 -> synLamExpr2 %~ updateExprPath sels
          SelLamArg   -> id

updatePiPath :: Path -> SynPi -> SynPi
updatePiPath path e =
  case uncons path of
    Nothing -> e & synPiSel .~ Nothing
    Just (toEnum -> sel', sels) -> e
      & synPiSel .~ Just sel'
      & case sel' of
          SelPiExpr1 -> synPiExpr1 %~ updateExprPath sels
          SelPiExpr2 -> synPiExpr2 %~ updateExprPath sels
          SelPiArg   -> id

updateAppPath :: Path -> SynApp -> SynApp
updateAppPath path e =
  case uncons path of
    Nothing -> e & synAppSel .~ Nothing
    Just (toEnum -> sel', sels) -> e
      & synAppSel .~ Just sel'
      & case sel' of
          SelAppExpr1 -> synAppExpr1 %~ updateExprPath sels
          SelAppExpr2 -> synAppExpr2 %~ updateExprPath sels

instance n ~ Int => SyntaxBlank (SynTop n) where
  blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    expr <- synImportExpr <$> case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> M.I.load e
    return $ SynTop (SynSolid expr) (pure 0) False [] []


---  helpers  ---

selLayout lctx (sel', synSub) hook syn =
  let
    lctx'
      = lctx
      & lctxSelected &&~ (synSelection syn == Just sel')
      & lctxPath %~ (`snoc` fromEnum sel')
  in sel (lctx' & lctxSelected &&~ synSelectionSelf (synSub syn))
   $ hook
   $ runReader (layout (synSub syn)) lctx'
