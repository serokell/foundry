module Foundry.Syn.Embed where

import Data.Foldable
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Source.Draw
import Source.Syntax
import Source.Input

import Foundry.Syn.Text
import Foundry.Syn.Common

data SynEmbed
  = SynEmbedFilePath SynText
  | SynEmbedURL SynText
  deriving (Eq, Ord, Show)

makePrisms ''SynEmbed

instance SyntaxSelection SynEmbed where
  selectionPath = const []

instance UndoEq SynEmbed where
  undoEq (SynEmbedFilePath t1) (SynEmbedFilePath t2) = undoEq t1 t2
  undoEq (SynEmbedURL      t1) (SynEmbedURL      t2) = undoEq t1 t2
  undoEq  _                     _                    = False

instance SyntaxLayout SynEmbed where
  layout (SynEmbedFilePath t) = reader $ \lctx ->
    collageWithMargin (Margin 4 4 4 4) $
    punct "file:" `horizTop` runReader (layout t) lctx
  layout (SynEmbedURL t) = reader $ \lctx ->
    collageWithMargin (Margin 4 4 4 4) $
    punct "url:" `horizTop` runReader (layout t) lctx

instance SyntaxReact SynEmbed where
  react = asum @[] handlers
    where
      handleShiftUp = do
        KeyPress [Shift] keyCode <- view rctxInputEvent
        guard $ keyLetter 'E' keyCode
        modify $ \case
          SynEmbedFilePath t -> SynEmbedURL t
          SynEmbedURL t -> SynEmbedFilePath t
      handlers =
        [ handleShiftUp
        , reactRedirect _SynEmbedURL
        , reactRedirect _SynEmbedFilePath ]
  subreact = simpleSubreact 'E' (SynEmbedURL mempty)
