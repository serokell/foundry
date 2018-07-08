module Foundry.Syn.Const where

import Control.Applicative
import Source.Syntax
import Foundry.Syn.Common

data SynConst = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

instance SynSelfSelected SynConst where
  synSelfSelected = const True

instance UndoEq SynConst where
  undoEq = (==)

instance n ~ Int => SyntaxLayout ActiveZone LayoutCtx SynConst where
  layout = pure . \case
    SynConstStar -> punct "★"
    SynConstBox  -> punct "□"

instance SyntaxReact rp ActiveZone SynConst where
  subreact
     =  simpleSubreact 'S' SynConstStar
    <|> simpleSubreact 'B' SynConstBox
