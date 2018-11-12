module Foundry.Syn.Expr where

import Numeric.Natural
import Data.Function

import Source.Draw
import Source.Path

import Foundry.Syn.Hole
import Foundry.Syn.Sum
import Foundry.Syn.Const
import Foundry.Syn.Embed
import Foundry.Syn.Arg
import Foundry.Syn.Var
import Foundry.Syn.Record
import Foundry.Syn.Common

type SynExpr = SynSum
  '[SynLam, SynPi, SynApp, SynConst, SynVar, SynEmbed]

---- Lam ----

data LabelLam

type SynLam = SynRecord LabelLam

type instance Fields LabelLam =
  '[ SynArg,
     SynHole SynExpr,
     SynHole SynExpr ]

instance SyntaxRecReact LabelLam where
  recChar = 'L'
  recDefaultValue =
    SynRecord (SynArg mempty :& SynHollow :& SynHollow :& RNil) IZ False

instance SyntaxRecLayout LabelLam where
  recLayout (arg :& expr1 :& expr2 :& RNil) =
    let
      maxWidth = (max `on` extentsW . collageExtents) header body
      header =
        [ punct "λ"
        , arg
        , punct ":"
        , expr1
        ] & horizontal
      body = expr2
    in
      collageWithMargin (Margin 4 4 4 4) $
      [ header
      , line light1 maxWidth
      , body
      ] & vertical

---- Pi ----

data LabelPi

type SynPi = SynRecord LabelPi

type instance Fields LabelPi =
  '[ SynArg,
     SynHole SynExpr,
     SynHole SynExpr ]

instance SyntaxRecReact LabelPi where
  recChar = 'P'
  recDefaultValue =
    SynRecord (SynArg mempty :& SynHollow :& SynHollow :& RNil) IZ False

instance SyntaxRecLayout LabelPi where
  recLayout (arg :& expr1 :& expr2 :& RNil) =
    let
      maxWidth = (max `on` extentsW . collageExtents) header body
      header =
        [ punct "Π"
        , arg
        , punct ":"
        , expr1
        ] & horizontal
      body = expr2
    in
      collageWithMargin (Margin 4 4 4 4) $
      [ header
      , line light1 maxWidth
      , body
      ] & vertical

---- App ----

data LabelApp

type SynApp = SynRecord LabelApp

type instance Fields LabelApp =
  '[ SynHole SynExpr,
     SynHole SynExpr ]

instance SyntaxRecReact LabelApp where
  recChar = 'A'
  recDefaultValue =
    SynRecord (SynHollow :& SynHollow :& RNil) IZ False

instance SyntaxRecLayout LabelApp where
  recLayout (expr1 :& expr2 :& RNil) =
    collageWithMargin (Margin 4 4 4 4) $
    [ expr1,
      substrate
        (lrtb @Natural 5 5 5 5)
        (rect (lrtb @Natural 1 1 1 1) (inj dark2))
        expr2
    ] & horizontalCenter
