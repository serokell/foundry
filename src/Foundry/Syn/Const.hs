module Foundry.Syn.Const where

import Control.Applicative
import Source.Syntax
import Source.Draw
import Foundry.Syn.Common

data SynConst = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

instance SyntaxSelection SynConst where
  selectionPath = const []

instance UndoEq SynConst where
  undoEq = (==)

instance n ~ Int => SyntaxLayout SynConst where
  layout =
    pure . collageWithMargin (Margin 4 4 4 4) .
    \case
      SynConstStar -> punct "★"
      SynConstBox  -> punct "□"

instance SyntaxReact SynConst where
  subreact
     =  simpleSubreact 'S' SynConstStar
    <|> simpleSubreact 'B' SynConstBox
