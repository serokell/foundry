{-# OPTIONS -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Void
import Data.Typeable

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Source.Collage.Builder (horizontal, vertical, getExtents)
import Source.Syntax
import Source.Draw
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Text
import Foundry.Syn.Common
import Foundry.Syn.TH

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

data EXPR_K = LAM | PI | APP | CONST | EMBED | VAR
data ARG
data EXPR
data TOP (n :: *)
data HOLE sub

data instance SEL LAM = SelLamArg | SelLamExpr1 | SelLamExpr2
  deriving (Eq, Ord, Show)

data instance SEL PI = SelPiArg | SelPiExpr1 | SelPiExpr2
  deriving (Eq, Ord, Show)

data instance SEL APP = SelAppExpr1 | SelAppExpr2
  deriving (Eq, Ord, Show)

declarePrisms
  [d|
    data instance SYN (HOLE sub) = SynSolid (SYN sub) | SynHollow
      deriving (Eq, Ord, Show)
  |]

data instance SYN LAM = SynLam
  { _synLamArg   :: SYN ARG
  , _synLamExpr1 :: SYN (HOLE EXPR)
  , _synLamExpr2 :: SYN (HOLE EXPR)
  , _synLamSel   :: Maybe (SEL LAM)
  } deriving (Eq, Ord, Show)

data instance SYN PI = SynPi
  { _synPiArg   :: SYN ARG
  , _synPiExpr1 :: SYN (HOLE EXPR)
  , _synPiExpr2 :: SYN (HOLE EXPR)
  , _synPiSel   :: Maybe (SEL PI)
  } deriving (Eq, Ord, Show)

data instance SYN APP = SynApp
  { _synAppExpr1 :: SYN (HOLE EXPR)
  , _synAppExpr2 :: SYN (HOLE EXPR)
  , _synAppSel   :: Maybe (SEL APP)
  } deriving (Eq, Ord, Show)

data instance SYN CONST = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

data instance SYN EMBED
  = SynEmbedFilePath (SYN TEXT)
  | SynEmbedURL (SYN TEXT)
  deriving (Eq, Ord, Show)

newtype instance SYN ARG = SynArg (SYN TEXT)
  deriving (Eq, Ord, Show)

data instance SYN VAR = SynVar
  { _synVarName  :: SYN TEXT
  , _synVarIndex :: Int
  } deriving (Eq, Ord, Show)

data instance SYN EXPR
  = SynExprLam   (SYN LAM)
  | SynExprPi    (SYN PI)
  | SynExprApp   (SYN APP)
  | SynExprConst (SYN CONST)
  | SynExprVar   (SYN VAR)
  | SynExprEmbed (SYN EMBED)
  deriving (Eq, Ord, Show)

declareLenses
  [d|
      data instance SYN (TOP n) = SynTop
        { synExpr            :: SYN (HOLE EXPR)
        , synPointer         :: Offset n
        , synHoverBarEnabled :: Bool
        , synUndo            :: [SYN (HOLE EXPR)]
        , synRedo            :: [SYN (HOLE EXPR)]
        } deriving (Eq, Show)
  |]

makeLensesDataInst ''SYN 'VAR
makePrismsDataInst ''SYN ''ARG
makeLensesDataInst ''SYN 'LAM
makeLensesDataInst ''SYN 'PI
makeLensesDataInst ''SYN 'APP
makePrismsDataInst ''SYN ''EXPR

synImportExpr :: M.Expr M.X -> SYN EXPR
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

class SynSelectionSelf a where
  synSelectionSelf :: SYN a -> Bool
  default synSelectionSelf :: SynSelection a => SYN a -> Bool
  synSelectionSelf = isNothing . synSelection

class SynSelectionSelf a => SynSelection a where
  synSelection :: SYN a -> Maybe (SEL a)
  synSelection = const Nothing

instance SynSelectionSelf LAM
instance SynSelection LAM where
  synSelection = view synLamSel

instance SynSelectionSelf PI
instance SynSelection PI where
  synSelection = view synPiSel

instance SynSelectionSelf APP
instance SynSelection APP where
  synSelection = view synAppSel

instance SynSelectionSelf ARG where
  synSelectionSelf _ = True

instance SynSelectionSelf CONST where
  synSelectionSelf _ = True

instance SynSelectionSelf VAR where
  synSelectionSelf _ = True

instance SynSelectionSelf EMBED where
  synSelectionSelf _ = True

instance SynSelectionSelf EXPR where
  synSelectionSelf = \case
    SynExprLam   a -> synSelectionSelf a
    SynExprPi    a -> synSelectionSelf a
    SynExprApp   a -> synSelectionSelf a
    SynExprConst a -> synSelectionSelf a
    SynExprVar   a -> synSelectionSelf a
    SynExprEmbed a -> synSelectionSelf a

instance SynSelectionSelf sub => SynSelectionSelf (HOLE sub) where
  synSelectionSelf = \case
    SynHollow -> True
    SynSolid syn -> synSelectionSelf syn

---       Arg       ---
---    instances    ---

instance UndoEq (SYN ARG) where
  undoEq (SynArg s1) (SynArg s2) = undoEq s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN ARG) where
  layout (SynArg t) = layout t

instance n ~ Int => SyntaxReact n ActiveZone (SYN ARG) where
  react = asum handlers
    where
      handlers = [reactRedirect _SynArg, handle_x]
      handle_x = do
        guardInputEvent $ keyCodeLetter KeyCode.Delete 'x'
        _SynArg .= mempty


---       Lam       ---
---    instances    ---

instance UndoEq (SYN LAM) where
  undoEq s1 s2
     = on undoEq (view synLamArg)   s1 s2
    && on undoEq (view synLamExpr1) s1 s2
    && on undoEq (view synLamExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN LAM) where
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

instance n ~ Int => SyntaxReact n ActiveZone (SYN LAM) where
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

instance SyntaxBlank (SYN LAM) where
  blank = return SynLam
    { _synLamArg   = SynArg mempty
    , _synLamExpr1 = SynHollow
    , _synLamExpr2 = SynHollow
    , _synLamSel   = Just SelLamArg
    }


---        Pi       ---
---    instances    ---

instance UndoEq (SYN PI) where
  undoEq s1 s2
     = on undoEq (view synPiArg)   s1 s2
    && on undoEq (view synPiExpr1) s1 s2
    && on undoEq (view synPiExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN PI) where
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

instance n ~ Int => SyntaxReact n ActiveZone (SYN PI) where
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

instance SyntaxBlank (SYN PI) where
  blank = return SynPi
    { _synPiArg   = SynArg mempty
    , _synPiExpr1 = SynHollow
    , _synPiExpr2 = SynHollow
    , _synPiSel   = Just SelPiArg
    }


---       App       ---
---    instances    ---

instance UndoEq (SYN APP) where
  undoEq s1 s2
     = on undoEq (view synAppExpr1) s1 s2
    && on undoEq (view synAppExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN APP) where
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

instance n ~ Int => SyntaxReact n ActiveZone (SYN APP) where
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

instance SyntaxBlank (SYN APP) where
  blank = return SynApp
    { _synAppExpr1 = SynHollow
    , _synAppExpr2 = SynHollow
    , _synAppSel   = Just SelAppExpr1
    }


---      Const      ---
---    instances    ---

instance UndoEq (SYN CONST) where
  undoEq = (==)

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN CONST) where

  layout = pure . \case
    SynConstStar -> punct "★"
    SynConstBox  -> punct "□"

instance n ~ Int => SyntaxReact n ActiveZone (SYN CONST) where

---       Var       ---
---    instances    ---

instance UndoEq (SYN VAR) where
  undoEq s1 s2
     = on undoEq (view synVarName)  s1 s2
    && on (==)   (view synVarIndex) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN VAR) where
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

instance n ~ Int => SyntaxReact n ActiveZone (SYN VAR) where
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

instance SyntaxBlank (SYN VAR) where
  blank = return $ SynVar mempty 0

---      Embed      ---
---    instances    ---

instance UndoEq (SYN EMBED) where
  undoEq (SynEmbedFilePath t1) (SynEmbedFilePath t2) = undoEq t1 t2
  undoEq (SynEmbedURL      t1) (SynEmbedURL      t2) = undoEq t1 t2
  undoEq  _                     _                    = False

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN EMBED) where
  layout _ = pure (text "Embed")

instance n ~ Int => SyntaxReact n ActiveZone (SYN EMBED) where

---       Expr      ---
---    instances    ---

instance UndoEq (SYN EXPR) where
  undoEq (SynExprLam   a1) (SynExprLam   a2) = undoEq a1 a2
  undoEq (SynExprPi    a1) (SynExprPi    a2) = undoEq a1 a2
  undoEq (SynExprApp   a1) (SynExprApp   a2) = undoEq a1 a2
  undoEq (SynExprConst a1) (SynExprConst a2) = undoEq a1 a2
  undoEq (SynExprVar   a1) (SynExprVar   a2) = undoEq a1 a2
  undoEq (SynExprEmbed a1) (SynExprEmbed a2) = undoEq a1 a2
  undoEq  _                 _                = False

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx (SYN EXPR) where
  layout = \case
    SynExprLam   a -> layout a
    SynExprPi    a -> layout a
    SynExprApp   a -> layout a
    SynExprConst a -> layout a
    SynExprVar   a -> layout a
    SynExprEmbed a -> layout a

instance n ~ Int => SyntaxReact n ActiveZone (SYN EXPR) where

  react = asum
    [ reactRedirect _SynExprLam
    , reactRedirect _SynExprPi
    , reactRedirect _SynExprApp
    , reactRedirect _SynExprConst
    , reactRedirect _SynExprVar
    , reactRedirect _SynExprEmbed ]

  subreact = asum handlers
    where
      handlers =
        [ handleShift_L
        , handleShift_P
        , handleShift_A
        , handleShift_S
        , handleShift_B
        , handle_i
        ] -- , handleShift_E
      handleShift_L = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'L' keyCode
        SynExprLam <$> liftIO blank
      handleShift_P = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'P' keyCode
        SynExprPi <$> liftIO blank
      handleShift_A = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'A' keyCode
        SynExprApp <$> liftIO blank
      handleShift_S = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'S' keyCode
        return $ SynExprConst SynConstStar
      handleShift_B = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'B' keyCode
        return $ SynExprConst SynConstBox
      handle_i = do
        KeyPress [] keyCode <- view rctxInputEvent
        guard $ keyLetter 'i' keyCode
        SynExprVar <$> liftIO blank


---       Hole      ---
---    instances    ---

instance UndoEq (SYN sub) => UndoEq (SYN (HOLE sub)) where
  undoEq (SynSolid s1) (SynSolid s2) = undoEq s1 s2
  undoEq  SynHollow     SynHollow    = True
  undoEq  _             _            = False

instance
  ( n ~ Int
  , SyntaxLayout n ActiveZone lctx (SYN sub)
  ) => SyntaxLayout n ActiveZone lctx (SYN (HOLE sub)) where

  layout = \case
    SynHollow    -> return (punct "_")
    SynSolid syn -> layout syn

instance
  ( n ~ Int
  , SyntaxReact n ActiveZone (SYN sub)
  ) => SyntaxReact n ActiveZone (SYN (HOLE sub)) where

  react = asum handlers
    where
      handlers =
        [ handleRedirectHollow
        , reactRedirect _SynSolid
        , handleDelete ]
      handleRedirectHollow = do
        SynHollow <- get
        subreactRedirect _SynSolid
      handleDelete = do
        guardInputEvent $ keyCodeLetter KeyCode.Delete 'x'
        put SynHollow

instance SyntaxBlank (SYN (HOLE sub)) where
  blank = return SynHollow


---       Top       ---
---    instances    ---

instance n ~ Int => SyntaxLayout n ActiveZone (Viewport n) (SYN (TOP n)) where
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

instance n ~ Int => SyntaxReact n ActiveZone (SYN (TOP n)) where
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
        Just p <- activate (\_ _ p -> p) pointer <$> view rctxLastLayout
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

updateExprPath :: Path -> SYN (HOLE EXPR) -> SYN (HOLE EXPR)
updateExprPath path
  = over _SynSolid
  $ over _SynExprLam (updateLamPath path)
  . over _SynExprPi  (updatePiPath  path)
  . over _SynExprApp (updateAppPath path)

updateLamPath :: Path -> SYN LAM -> SYN LAM
updateLamPath path e =
  case uncons path of
    Nothing -> e & synLamSel .~ Nothing
    Just (SomeSel sel', someSels) ->
      let msel = cast sel'
      in e
       & synLamSel .~ msel
       & case msel of
          Just SelLamExpr1 -> synLamExpr1 %~ updateExprPath someSels
          Just SelLamExpr2 -> synLamExpr2 %~ updateExprPath someSels
          _ -> id

updatePiPath :: Path -> SYN PI -> SYN PI
updatePiPath path e =
  case uncons path of
    Nothing -> e & synPiSel .~ Nothing
    Just (SomeSel sel', someSels) ->
      let msel = cast sel'
      in e
       & synPiSel .~ msel
       & case msel of
          Just SelPiExpr1 -> synPiExpr1 %~ updateExprPath someSels
          Just SelPiExpr2 -> synPiExpr2 %~ updateExprPath someSels
          _ -> id

updateAppPath :: Path -> SYN APP -> SYN APP
updateAppPath path e =
  case uncons path of
    Nothing -> e & synAppSel .~ Nothing
    Just (SomeSel sel', someSels) ->
      let msel = cast sel'
      in e
       & synAppSel .~ msel
       & case msel of
          Nothing -> id
          Just SelAppExpr1 -> synAppExpr1 %~ updateExprPath someSels
          Just SelAppExpr2 -> synAppExpr2 %~ updateExprPath someSels

instance n ~ Int => SyntaxBlank (SYN (TOP n)) where
  blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    expr <- synImportExpr <$> case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> M.I.load e
    return $ SynTop (SynSolid expr) (pure 0) False [] []


---  helpers  ---

guardInputEvent :: (InputEvent n -> Bool) -> React n la syn
guardInputEvent = guard <=< views rctxInputEvent

selLayout lctx (sel', synSub) hook syn =
  let
    lctx'
      = lctx
      & lctxSelected &&~ (synSelection syn == Just sel')
      & lctxPath %~ (`snoc` SomeSel sel')
  in sel (lctx' & lctxSelected &&~ synSelectionSelf (synSub syn))
   $ hook
   $ runReader (layout (synSub syn)) lctx'
