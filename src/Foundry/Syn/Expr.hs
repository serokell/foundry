module Foundry.Syn.Expr where

import Data.Function
import Control.Applicative
import Control.Monad.Reader
import Control.Lens
import Data.Dynamic

import qualified Language.Haskell.TH as TH
import qualified Data.Singletons.TH as Sing
import Data.Singletons.Prelude

import Data.Vinyl

import Source.Collage.Builder (horizontal, vertical, getExtents)
import Source.Syntax
import Source.Draw
import Source.Input

import Foundry.Syn.Hole
import Foundry.Syn.Sum
import Foundry.Syn.Const
import Foundry.Syn.Embed
import Foundry.Syn.Arg
import Foundry.Syn.Var
import Foundry.Syn.Record
import Foundry.Syn.Common

Sing.singletons [d|
  data SelLam = SelLamArg | SelLamExpr1 | SelLamExpr2
    deriving (Eq, Ord, Enum, Bounded, Show)

  data SelPi = SelPiArg | SelPiExpr1 | SelPiExpr2
    deriving (Eq, Ord, Enum, Bounded, Show)

  data SelApp = SelAppExpr1 | SelAppExpr2
    deriving (Eq, Ord, Enum, Bounded, Show)
  |]

-- A necessary hack. Does nothing.
TH.runQ $ return []

instance Sel SelLam
instance Sel SelPi
instance Sel SelApp

type instance FieldType (s :: SelLam) = FieldTypes s
  '[SynArg, SynHole SynExpr, SynHole SynExpr]

type instance FieldType (s :: SelPi) = FieldTypes s
  '[SynArg, SynHole SynExpr, SynHole SynExpr]

type instance FieldType (s :: SelApp) = FieldTypes s
  '[SynHole SynExpr, SynHole SynExpr]

type SynExpr = SynSum
  '[SynLam, SynPi, SynApp, SynConst, SynVar, SynEmbed]

type SynLam = SynRecord ('KProxy :: KProxy SelLam)
type SynPi = SynRecord ('KProxy :: KProxy SelPi)
type SynApp = SynRecord ('KProxy :: KProxy SelApp)

---       Lam       ---
---    instances    ---

instance SelLayout SelLam where
  selLayoutHook = \case
    SelLamArg   -> join pad (Point 4 0)
    SelLamExpr1 -> join pad (Point 4 0)
    SelLamExpr2 -> id

instance UndoEq SynLam where
  undoEq s1 s2
     = on undoEq (view (synField SSelLamArg))   s1 s2
    && on undoEq (view (synField SSelLamExpr1)) s1 s2
    && on undoEq (view (synField SSelLamExpr2)) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynLam where
  layout syn = reader $ \lctx ->
    let
      maxWidth = (max `on` view pointX . getExtents) header body
      header =
        [ extend (Point 4 0) (punct "λ")
        , [ runReader (selLayout SSelLamArg syn) lctx
          , join pad (Point 4 0) (punct ":")
          , runReader (selLayout SSelLamExpr1 syn) lctx
          ] & horizontal
        ] & horizontal
      body = runReader (selLayout SSelLamExpr2 syn) lctx
    in
      [ header
      , join pad (Point 0 4) (line light1 maxWidth)
      , body
      ] & vertical

instance n ~ Int => SyntaxReact n rp ActiveZone SynLam where
  react = handleSelRedirect <|> handleArrows
    where
      handleSelRedirect :: React n rp ActiveZone SynLam
      handleSelRedirect = do
        False <- use synSelectionSelf
        SomeSing selection <- use synRecSel
        $(Sing.sCases ''SelLam [e|selection|]
            [e|reactRedirect (synField selection)|])
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'L' keyCode
    return $ SynRecord
       ( SynRecField (SynArg mempty)
      :& SynRecField SynHollow
      :& SynRecField SynHollow
      :& RNil )
      (toSing SelLamArg)
      False

---        Pi       ---
---    instances    ---

instance SelLayout SelPi where
  selLayoutHook = \case
    SelPiArg   -> join pad (Point 4 0)
    SelPiExpr1 -> join pad (Point 4 0)
    SelPiExpr2 -> id

instance UndoEq SynPi where
  undoEq s1 s2
     = on undoEq (view (synField SSelPiArg))   s1 s2
    && on undoEq (view (synField SSelPiExpr1)) s1 s2
    && on undoEq (view (synField SSelPiExpr2)) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynPi where
  layout syn = reader $ \lctx ->
    let
      maxWidth = (max `on` view pointX . getExtents) header body
      header =
        [ extend (Point 4 0) (punct "Π")
        , [ runReader (selLayout SSelPiArg syn) lctx
          , join pad (Point 4 0) (punct ":")
          , runReader (selLayout SSelPiExpr1 syn) lctx
          ] & horizontal
        ] & horizontal
      body = runReader (selLayout SSelPiExpr2 syn) lctx
    in
      [ header
      , join pad (Point 0 4) (line light1 maxWidth)
      , body
      ] & vertical

instance n ~ Int => SyntaxReact n rp ActiveZone SynPi where
  react = handleSelRedirect <|> handleArrows
    where
      handleSelRedirect :: React n rp ActiveZone SynPi
      handleSelRedirect = do
        False <- use synSelectionSelf
        SomeSing selection <- use synRecSel
        $(Sing.sCases ''SelPi [e|selection|]
            [e|reactRedirect (synField selection)|])
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'P' keyCode
    return $ SynRecord
       ( SynRecField (SynArg mempty)
      :& SynRecField SynHollow
      :& SynRecField SynHollow
      :& RNil )
      (toSing SelPiArg)
      False

---       App       ---
---    instances    ---

instance SelLayout SelApp where
  selLayoutHook = \case
    SelAppExpr1 -> join pad (Point 5 5)
    SelAppExpr2 -> outline dark2 . join pad (Point 5 5)

instance UndoEq SynApp where
  undoEq s1 s2
     = on undoEq (view (synField SSelAppExpr1)) s1 s2
    && on undoEq (view (synField SSelAppExpr2)) s1 s2

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynApp where
  layout syn = reader $ \lctx ->
    [ runReader (selLayout SSelAppExpr1 syn) lctx
    , join pad (Point 5 5)
      $ runReader (selLayout SSelAppExpr2 syn) lctx
    ] & horizontalCenter

instance n ~ Int => SyntaxReact n rp ActiveZone SynApp where
  react = handleSelRedirect <|> handleArrows
    where
      handleSelRedirect :: React n rp ActiveZone SynApp
      handleSelRedirect = do
        False <- use synSelectionSelf
        SomeSing selection <- use synRecSel
        $(Sing.sCases ''SelApp [e|selection|]
            [e|reactRedirect (synField selection)|])
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    guard $ keyLetter 'A' keyCode
    return $ SynRecord
      (SynRecField SynHollow :& SynRecField SynHollow :& RNil)
      (toSing SelAppExpr1) False

---  helpers  ---

selLayout ssel syn = do
  let
    sel' = fromSing ssel
    sub = view (synField ssel) syn
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
