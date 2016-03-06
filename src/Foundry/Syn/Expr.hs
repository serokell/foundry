module Foundry.Syn.Expr where

import Data.Foldable
import Data.Function
import Control.Applicative
import Control.Monad.Reader
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
  , _synLamSel :: SelLam
  , _synLamSelSelf :: Bool
  } deriving (Eq, Ord, Show)

data SelPi = SelPiArg | SelPiExpr1 | SelPiExpr2
  deriving (Eq, Ord, Enum, Bounded, Show)

data SynPi = SynPi
  { _synPiArg   :: SynArg
  , _synPiExpr1 :: SynHole SynExpr
  , _synPiExpr2 :: SynHole SynExpr
  , _synPiSel :: SelPi
  , _synPiSelSelf :: Bool
  } deriving (Eq, Ord, Show)

data SelApp = SelAppExpr1 | SelAppExpr2
  deriving (Eq, Ord, Enum, Bounded, Show)

data SynApp = SynApp
  { _synAppExpr1 :: SynHole SynExpr
  , _synAppExpr2 :: SynHole SynExpr
  , _synAppSel :: SelApp
  , _synAppSelSelf :: Bool
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

selNext, selPrev :: Sel s => s -> Maybe s
selNext = lookupNext selOrder
selPrev = lookupNext selRevOrder

handleArrowUp :: SynSelection syn sel => React n rp la syn
handleArrowUp = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
  False <- use synSelectionSelf
  synSelectionSelf .= True

handleArrowDown :: SynSelection syn sel => React n rp la syn
handleArrowDown = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
  True <- use synSelectionSelf
  synSelectionSelf .= False

handleArrowLeft
  :: (SynSelection syn sel, Sel sel)
  => React n rp la syn
handleArrowLeft = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (selPrev selection)
  synSelection .= selection'

handleArrowRight
  :: (SynSelection syn sel, Sel sel)
  => React n rp la syn
handleArrowRight = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (selNext selection)
  synSelection .= selection'

handleArrows
  :: (SynSelection syn sel, Sel sel)
  => React n rp la syn
handleArrows = asum
  [handleArrowUp, handleArrowDown, handleArrowLeft, handleArrowRight]

class SelLayout s where
  selLayoutHook :: s -> Op1 (CollageDraw' Int)

instance SelLayout SelLam where
  selLayoutHook = \case
    SelLamArg   -> join pad (Point 4 0)
    SelLamExpr1 -> join pad (Point 4 0)
    SelLamExpr2 -> id

instance SelLayout SelPi where
  selLayoutHook = \case
    SelPiArg   -> join pad (Point 4 0)
    SelPiExpr1 -> join pad (Point 4 0)
    SelPiExpr2 -> id

instance SelLayout SelApp where
  selLayoutHook = \case
    SelAppExpr1 -> join pad (Point 5 5)
    SelAppExpr2 -> outline dark2 . join pad (Point 5 5)

---       Lam       ---
---    instances    ---

instance SynSelfSelected SynLam
instance SynSelection SynLam SelLam where
  synSelection = synLamSel
  synSelectionSelf = synLamSelSelf

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
        , [ runReader (selLayout (SelLamArg, synLamArg) syn) lctx
          , join pad (Point 4 0) (punct ":")
          , runReader (selLayout (SelLamExpr1, synLamExpr1) syn) lctx
          ] & horizontal
        ] & horizontal
      body = runReader (selLayout (SelLamExpr2, synLamExpr2) syn) lctx
    in
      [ header
      , join pad (Point 0 4) (line light1 maxWidth)
      , body
      ] & vertical

instance n ~ Int => SyntaxReact n rp ActiveZone SynLam where
  react = handleSelRedirect <|> handleArrows
    where
      handleSelRedirect = do
        False <- use synSelectionSelf
        use synSelection >>= \case
          SelLamArg   -> reactRedirect synLamArg
          SelLamExpr1 -> reactRedirect synLamExpr1
          SelLamExpr2 -> reactRedirect synLamExpr2
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'L' keyCode
    return $ SynLam (SynArg mempty) SynHollow SynHollow SelLamArg False

---        Pi       ---
---    instances    ---

instance SynSelfSelected SynPi
instance SynSelection SynPi SelPi where
  synSelection = synPiSel
  synSelectionSelf = synPiSelSelf

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
        , [ runReader (selLayout (SelPiArg, synPiArg) syn) lctx
          , join pad (Point 4 0) (punct ":")
          , runReader (selLayout (SelPiExpr1, synPiExpr1) syn) lctx
          ] & horizontal
        ] & horizontal
      body = runReader (selLayout (SelPiExpr2, synPiExpr2) syn) lctx
    in
      [ header
      , join pad (Point 0 4) (line light1 maxWidth)
      , body
      ] & vertical

instance n ~ Int => SyntaxReact n rp ActiveZone SynPi where
  react = handleSelRedirect <|> handleArrows
    where
      handleSelRedirect = do
        False <- use synSelectionSelf
        use synSelection >>= \case
          SelPiArg   -> reactRedirect synPiArg
          SelPiExpr1 -> reactRedirect synPiExpr1
          SelPiExpr2 -> reactRedirect synPiExpr2
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'P' keyCode
    return $ SynPi (SynArg mempty) SynHollow SynHollow SelPiArg False


---       App       ---
---    instances    ---

instance SynSelfSelected SynApp
instance SynSelection SynApp SelApp where
  synSelection = synAppSel
  synSelectionSelf = synAppSelSelf

instance UndoEq SynApp where
  undoEq s1 s2
     = on undoEq (view synAppExpr1) s1 s2
    && on undoEq (view synAppExpr2) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynApp where
  layout syn = reader $ \lctx ->
    [ runReader (selLayout (SelAppExpr1, synAppExpr1) syn) lctx
    , join pad (Point 5 5)
      $ runReader (selLayout (SelAppExpr2, synAppExpr2) syn) lctx
    ] & horizontalCenter

instance n ~ Int => SyntaxReact n rp ActiveZone SynApp where
  react = handleSelRedirect <|> handleArrows
    where
      handleSelRedirect = do
        False <- use synSelectionSelf
        use synSelection >>= \case
          SelAppExpr1 -> reactRedirect synAppExpr1
          SelAppExpr2 -> reactRedirect synAppExpr2
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'A' keyCode
    return $ SynApp SynHollow SynHollow SelAppExpr1 False

---  helpers  ---

selLayout (sel', synSub) syn = do
  let
    sub = view synSub syn
    appendSelection
      = (lctxSelected &&~ (view synSelection syn == sel'))
      . (lctxSelected &&~ (synSelfSelected syn == False))
      . (lctxPath %~ (`snoc` toDyn sel'))
    enforceSelfSelection
      = lctxSelected &&~ synSelfSelected sub
  local appendSelection $ do
    a <- layout sub
    reader $ \lctx ->
       sel (enforceSelfSelection lctx)
     $ selLayoutHook sel' a
