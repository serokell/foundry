module Foundry.Syn.Hole where

import Control.Monad.State
import Control.Lens
import Data.Foldable

import Source.Syntax
import Source.Draw
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Common

data SynHole sub = SynSolid sub | SynHollow
  deriving (Eq, Ord, Show)

makePrisms ''SynHole

instance SyntaxSelection sub => SyntaxSelection (SynHole sub) where
  selectionPath = \case
    SynHollow -> []
    SynSolid syn -> selectionPath syn

instance UndoEq sub => UndoEq (SynHole sub) where
  undoEq (SynSolid s1) (SynSolid s2) = undoEq s1 s2
  undoEq  SynHollow     SynHollow    = True
  undoEq  _             _            = False

instance SyntaxLayout sub => SyntaxLayout (SynHole sub) where
  layout = \case
    SynHollow -> return $
      collageWithMargin (Margin 4 4 4 4) $
      punct "_"
    SynSolid syn -> layout syn

instance SyntaxReact sub => SyntaxReact (SynHole sub) where
  react = asum @[] handlers
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
