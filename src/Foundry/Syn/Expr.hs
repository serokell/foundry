module Foundry.Syn.Expr where

import Data.Foldable
import Data.Function
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.Dynamic

import Source.Collage.Builder (horizontal, vertical, getExtents)
import Source.Syntax
import Source.Draw
import Source.Input
import qualified Source.Input.KeyCode as KeyCode

import Foundry.Syn.Hole
import Foundry.Syn.Sum
import Foundry.Syn.Const
import Foundry.Syn.Embed
import Foundry.Syn.Arg
import Foundry.Syn.Var
import Foundry.Syn.Common

type SynExpr = SynSum
  '[SynLam, SynPi, SynApp, SynConst, SynVar, SynEmbed]

data SelLam = SelLamArg | SelLamExpr1 | SelLamExpr2
  deriving (Eq, Ord, Enum, Bounded, Show)

data SynLam = SynLam
  { _synLamArg   :: SynArg
  , _synLamExpr1 :: SynHole SynExpr
  , _synLamExpr2 :: SynHole SynExpr
  , _synLamSel   :: Maybe SelLam
  } deriving (Eq, Ord, Show)

data SelPi = SelPiArg | SelPiExpr1 | SelPiExpr2
  deriving (Eq, Ord, Enum, Bounded, Show)

data SynPi = SynPi
  { _synPiArg   :: SynArg
  , _synPiExpr1 :: SynHole SynExpr
  , _synPiExpr2 :: SynHole SynExpr
  , _synPiSel   :: Maybe SelPi
  } deriving (Eq, Ord, Show)

data SelApp = SelAppExpr1 | SelAppExpr2
  deriving (Eq, Ord, Enum, Bounded, Show)

data SynApp = SynApp
  { _synAppExpr1 :: SynHole SynExpr
  , _synAppExpr2 :: SynHole SynExpr
  , _synAppSel   :: Maybe SelApp
  } deriving (Eq, Ord, Show)

makeLenses ''SynLam
makeLenses ''SynPi
makeLenses ''SynApp

class Eq s => Sel s where
  selOrder :: [s]
  default selOrder :: (Enum s, Bounded s) => [s]
  selOrder = [minBound .. maxBound]

instance Sel SelLam
instance Sel SelPi
instance Sel SelApp

lookupNext :: Eq s => [s] -> s -> Maybe s
lookupNext ss s = lookup s (ss `zip` tail ss)

selRevOrder :: Sel s => [s]
selRevOrder = reverse selOrder

selNext :: Sel s => s -> Maybe s
selNext = lookupNext selOrder

selPrev :: Sel s => s -> Maybe s
selPrev = lookupNext selRevOrder

---       Lam       ---
---    instances    ---

instance SynSelection SynLam SelLam where
  synSelection = view synLamSel

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
        Just selection <- gets synSelection
        selection' <- maybeA (selPrev selection)
        synLamSel . _Just .= selection'
      handleArrowRight = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
        Just selection <- gets synSelection
        selection' <- maybeA (selNext selection)
        synLamSel . _Just .= selection'
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

instance SynSelection SynPi SelPi where
  synSelection = view synPiSel

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
        Just selection <- gets synSelection
        selection' <- maybeA (selPrev selection)
        synPiSel . _Just .= selection'
      handleArrowRight = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
        Just selection <- gets synSelection
        selection' <- maybeA (selNext selection)
        synPiSel . _Just .= selection'
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

instance SynSelection SynApp SelApp where
  synSelection = view synAppSel

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
        Just selection <- gets synSelection
        selection' <- maybeA (selPrev selection)
        synAppSel . _Just .= selection'
      handleArrowRight = do
        guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
        Just selection <- gets synSelection
        selection' <- maybeA (selNext selection)
        synAppSel . _Just .= selection'
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

---  helpers  ---

selLayout
  :: (Typeable a1, Eq a1, SynSelection a a1, SynSelection syn sel,
      SyntaxLayout n la LayoutCtx syn)
  => LayoutCtx
  -> (a1, a -> syn)
  -> (CollageBuilderDraw n la -> CollageDraw' Int)
  -> a
  -> CollageDraw' Int
selLayout lctx (sel', synSub) hook syn =
  let
    lctx'
      = lctx
      & lctxSelected &&~ (synSelection syn == Just sel')
      & lctxPath %~ (`snoc` toDyn sel')
  in sel (lctx' & lctxSelected &&~ synSelectionSelf (synSub syn))
   $ hook
   $ runReader (layout (synSub syn)) lctx'
