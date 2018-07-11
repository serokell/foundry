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

instance SynSelfSelected sub => SynSelfSelected (SynHole sub) where
  synSelfSelected = \case
    SynHollow -> True
    SynSolid syn -> synSelfSelected syn

instance UndoEq sub => UndoEq (SynHole sub) where
  undoEq (SynSolid s1) (SynSolid s2) = undoEq s1 s2
  undoEq  SynHollow     SynHollow    = True
  undoEq  _             _            = False

instance (SyntaxLayout Path lctx sub)
      => SyntaxLayout Path lctx (SynHole sub) where
  layout = \case
    SynHollow    -> return (punct "_")
    SynSolid syn -> layout syn

instance SyntaxReact rp Path sub
      => SyntaxReact rp Path (SynHole sub) where
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
