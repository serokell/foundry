{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Hole where

import Control.Monad.State
import Control.Lens
import Data.Foldable

import Source.Syntax
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Common

data SynHole sub = SynSolid sub | SynHollow
  deriving (Eq, Ord, Show)

makePrisms ''SynHole

instance SynSelection sub sel => SynSelection (SynHole sub) sel where
  synSelection = \case
    SynHollow -> Nothing
    SynSolid syn -> synSelection syn

instance UndoEq sub => UndoEq (SynHole sub) where
  undoEq (SynSolid s1) (SynSolid s2) = undoEq s1 s2
  undoEq  SynHollow     SynHollow    = True
  undoEq  _             _            = False

instance (n ~ Int, SyntaxLayout n ActiveZone lctx sub)
      => SyntaxLayout n ActiveZone lctx (SynHole sub) where
  layout = \case
    SynHollow    -> return (punct "_")
    SynSolid syn -> layout syn

instance SyntaxReact n ActiveZone sub
      => SyntaxReact n ActiveZone (SynHole sub) where
  react = asum handlers
    where
      handlers =
        [ reactRedirect _SynSolid
        , handleRedirectHollow
        , handleDelete ]
      handleRedirectHollow = do
        SynHollow <- get
        subreactToReact subreact
      handleDelete = do
        guardInputEvent $ keyCodeLetter KeyCode.Delete 'x'
        put SynHollow
  subreact = subreactRedirect _SynSolid

instance SyntaxBlank (SynHole sub) where
  blank = return SynHollow
