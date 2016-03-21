module Foundry.Syn.Expr where

import Data.Function
import Control.Applicative
import Control.Monad.Reader
import Control.Lens

import qualified Language.Haskell.TH as TH
import qualified Data.Singletons.TH as Sing
import Data.Singletons.Prelude

import Data.Vinyl

import Source.Collage.Builder (horizontal, vertical, getExtents)
import Source.Syntax
import Source.Draw

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

type SynExpr = SynSum
  '[SynLam, SynPi, SynApp, SynConst, SynVar, SynEmbed]


---- Lam ----

type SynLam = SynRecord ('KProxy :: KProxy SelLam)

type instance FieldType 'SelLamArg   = SynArg
type instance FieldType 'SelLamExpr1 = SynHole SynExpr
type instance FieldType 'SelLamExpr2 = SynHole SynExpr

instance Sel SelLam

instance SelLayout SelLam where
  selLayoutHook = \case
    SelLamArg   -> join pad (Point 4 0)
    SelLamExpr1 -> join pad (Point 4 0)
    SelLamExpr2 -> id

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
  react = $(recHandleSelRedirect ''SelLam) <|> handleArrows
  subreact
    = simpleSubreact 'L'
    $ SynRecord
       ( SynRecField (SynArg mempty)
      :& SynRecField SynHollow
      :& SynRecField SynHollow
      :& RNil )
      (toSing SelLamArg)
      False


---- Pi ----

type SynPi = SynRecord ('KProxy :: KProxy SelPi)

type instance FieldType 'SelPiArg   = SynArg
type instance FieldType 'SelPiExpr1 = SynHole SynExpr
type instance FieldType 'SelPiExpr2 = SynHole SynExpr

instance Sel SelPi

instance SelLayout SelPi where
  selLayoutHook = \case
    SelPiArg   -> join pad (Point 4 0)
    SelPiExpr1 -> join pad (Point 4 0)
    SelPiExpr2 -> id

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
  react = $(recHandleSelRedirect ''SelPi) <|> handleArrows
  subreact
    = simpleSubreact 'P'
    $ SynRecord
       ( SynRecField (SynArg mempty)
      :& SynRecField SynHollow
      :& SynRecField SynHollow
      :& RNil )
      (toSing SelPiArg)
      False


---- App ----

type SynApp = SynRecord ('KProxy :: KProxy SelApp)

type instance FieldType 'SelAppExpr1 = SynHole SynExpr
type instance FieldType 'SelAppExpr2 = SynHole SynExpr

instance Sel SelApp

instance SelLayout SelApp where
  selLayoutHook = \case
    SelAppExpr1 -> join pad (Point 5 5)
    SelAppExpr2 -> outline dark2 . join pad (Point 5 5)

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynApp where
  layout syn = reader $ \lctx ->
    [ runReader (selLayout SSelAppExpr1 syn) lctx
    , join pad (Point 5 5)
      $ runReader (selLayout SSelAppExpr2 syn) lctx
    ] & horizontalCenter

instance n ~ Int => SyntaxReact n rp ActiveZone SynApp where
  react = $(recHandleSelRedirect ''SelApp) <|> handleArrows
  subreact
    = simpleSubreact 'A'
    $ SynRecord
      (SynRecField SynHollow :& SynRecField SynHollow :& RNil)
      (toSing SelAppExpr1) False
