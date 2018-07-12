module Foundry.Syn.Expr where

import Numeric.Natural
import Data.Function
import Control.Applicative
import Control.Monad.Reader

import qualified Data.Singletons.TH as Sing
import Data.Singletons.Prelude

import Data.Constraint
import Data.Vinyl hiding (Dict)

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

return []

type SynExpr = SynSum
  '[SynLam, SynPi, SynApp, SynConst, SynVar, SynEmbed]


---- Lam ----

type SynLam = SynRecord SelLam

type instance FieldTypes SelLam =
  '[ SynArg
   , SynHole SynExpr
   , SynHole SynExpr ]

instance SelLayout Path SelLam where
  selLayoutC = \case
    SSelLamArg -> Dict
    SSelLamExpr1 -> Dict
    SSelLamExpr2 -> Dict
  selLayoutHook = \case
    SelLamArg   -> pad (LRTB 4 4 0 0)
    SelLamExpr1 -> pad (LRTB 4 4 0 0)
    SelLamExpr2 -> id

instance SyntaxLayout Path LayoutCtx SynLam where
  layout syn = reader $ \lctx ->
    let
      maxWidth = (max `on` extentsW . collageExtents) header body
      header =
        [ pad (LRTB 0 4 0 0) (punct "λ")
        , [ runReader (selLayout SSelLamArg syn) lctx
          , pad (LRTB 4 4 0 0) (punct ":")
          , runReader (selLayout SSelLamExpr1 syn) lctx
          ] & horizontal
        ] & horizontal
      body = runReader (selLayout SSelLamExpr2 syn) lctx
    in
      [ header
      , pad (LRTB 0 0 4 4) (line light1 maxWidth)
      , body
      ] & vertical

instance SyntaxReact rp Path SynLam where
  react = recHandleSelRedirect <|> handleArrows
  subreact
    = simpleSubreact 'L'
    $ SynRecord
       ( SynRecField (SynArg mempty)
      :& SynRecField SynHollow
      :& SynRecField SynHollow
      :& RNil )
      0
      False


---- Pi ----

type SynPi = SynRecord SelPi

type instance FieldTypes SelPi =
  '[ SynArg
   , SynHole SynExpr
   , SynHole SynExpr ]

instance SelLayout Path SelPi where
  selLayoutC = \case
    SSelPiArg -> Dict
    SSelPiExpr1 -> Dict
    SSelPiExpr2 -> Dict
  selLayoutHook = \case
    SelPiArg   -> pad (LRTB 4 4 0 0)
    SelPiExpr1 -> pad (LRTB 4 4 0 0)
    SelPiExpr2 -> id

instance SyntaxLayout Path LayoutCtx SynPi where
  layout syn = reader $ \lctx ->
    let
      maxWidth = (max `on` extentsW . collageExtents) header body
      header =
        [ pad (LRTB 0 4 0 0) (punct "Π")
        , [ runReader (selLayout SSelPiArg syn) lctx
          , pad (LRTB 4 4 0 0) (punct ":")
          , runReader (selLayout SSelPiExpr1 syn) lctx
          ] & horizontal
        ] & horizontal
      body = runReader (selLayout SSelPiExpr2 syn) lctx
    in
      [ header
      , pad (LRTB 0 0 4 4) (line light1 maxWidth)
      , body
      ] & vertical

instance SyntaxReact rp Path SynPi where
  react = recHandleSelRedirect <|> handleArrows
  subreact
    = simpleSubreact 'P'
    $ SynRecord
       ( SynRecField (SynArg mempty)
      :& SynRecField SynHollow
      :& SynRecField SynHollow
      :& RNil )
      0
      False


---- App ----

type SynApp = SynRecord SelApp

type instance FieldTypes SelApp =
  '[ SynHole SynExpr
   , SynHole SynExpr ]

instance SelLayout Path SelApp where
  selLayoutC = \case
    SSelAppExpr1 -> Dict
    SSelAppExpr2 -> Dict
  selLayoutHook = \case
    SelAppExpr1 ->
      pad (LRTB 5 5 5 5)
    SelAppExpr2 ->
      substrate
        (lrtb @Natural 5 5 5 5)
        (rect (lrtb @Natural 1 1 1 1) (inj dark2))

instance SyntaxLayout Path LayoutCtx SynApp where
  layout syn = reader $ \lctx ->
    [ runReader (selLayout SSelAppExpr1 syn) lctx
    , pad (LRTB 5 5 5 5)
      $ runReader (selLayout SSelAppExpr2 syn) lctx
    ] & horizontalCenter

instance SyntaxReact rp Path SynApp where
  react = recHandleSelRedirect <|> handleArrows
  subreact
    = simpleSubreact 'A'
    $ SynRecord
      (SynRecField SynHollow :& SynRecField SynHollow :& RNil)
      0 False
