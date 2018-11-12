module Foundry.Syn.Arg where

import Data.Foldable
import Control.Lens

import Source.Syntax
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Text
import Foundry.Syn.Common

newtype SynArg = SynArg SynText
  deriving (Eq, Ord, Show)

makePrisms ''SynArg

instance UndoEq SynArg where
  undoEq (SynArg s1) (SynArg s2) = undoEq s1 s2

instance SyntaxSelection SynArg where
  selectionPath = const []

instance SyntaxLayout SynArg where
  layout (SynArg t) = layout t

instance SyntaxReact SynArg where
  react = asum @[] handlers
    where
      handlers = [reactRedirect _SynArg, handle_x]
      handle_x = do
        guardInputEvent $ keyCodeLetter KeyCode.Delete 'x'
        _SynArg .= mempty
