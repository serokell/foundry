module Foundry.Syn.Var where

import qualified Data.Text as Text
import Data.Foldable
import Data.Function
import Control.Monad.Reader
import Control.Lens

import Source.Draw
import Source.Syntax
import Source.Input
import Foundry.Syn.Text
import Foundry.Syn.Common

data SynVar = SynVar
  { _synVarName  :: SynText
  , _synVarIndex :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''SynVar

instance SynSelfSelected SynVar where
  synSelfSelected = const True

instance UndoEq SynVar where
  undoEq s1 s2
     = on undoEq (view synVarName)  s1 s2
    && on (==)   (view synVarIndex) s1 s2

instance SyntaxLayout Path LayoutCtx SynVar where
  layout v = reader $ \lctx ->
    let
      c = runReader (layout (v ^. synVarName)) lctx
    in
      if (v ^. synVarIndex) > 0
      then c `horizTop` layoutIndex (v ^. synVarIndex)
      else c
    where
      layoutIndex = text . Text.map toSub . Text.pack . show
      toSub :: Char -> Char
      toSub = \case
        '0' -> '₀'
        '1' -> '₁'
        '2' -> '₂'
        '3' -> '₃'
        '4' -> '₄'
        '5' -> '₅'
        '6' -> '₆'
        '7' -> '₇'
        '8' -> '₈'
        '9' -> '₉'
        c   -> c

instance SyntaxReact rp Path SynVar where
  react = asum @[] handlers
    where
      handlers =
        [ handleShiftUp
        , handleShiftDown
        , reactRedirect synVarName ]
      handleShiftUp = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'U' keyCode
        synVarIndex += 1
      handleShiftDown = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'D' keyCode
        synVarIndex %= max 0 . subtract 1
  subreact = do
    KeyPress [] keyCode <- view rctxInputEvent
    guard $ keyLetter 'i' keyCode
    return $ SynVar mempty 0
