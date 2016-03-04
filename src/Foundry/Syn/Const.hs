module Foundry.Syn.Const where

import Data.Void
import Control.Monad.Reader
import Control.Lens

import Source.Syntax
import Source.Input

import Foundry.Syn.Common

data SynConst = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

instance SynSelection SynConst Void

instance UndoEq SynConst where
  undoEq = (==)

instance n ~ Int => SyntaxLayout n ActiveZone LayoutCtx SynConst where
  layout = pure . \case
    SynConstStar -> punct "★"
    SynConstBox  -> punct "□"

instance n ~ Int => SyntaxReact n ActiveZone SynConst where
  subreact = do
    KeyPress [Shift] keyCode <- view rctxInputEvent
    if | keyLetter 'S' keyCode -> return SynConstStar
       | keyLetter 'B' keyCode -> return SynConstBox
       | otherwise -> mzero
