module Foundry.Syn.Text where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import Data.Function

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import qualified Graphics.UI.Gtk as Gtk

import Source.Draw
import Source.Syntax
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Common

data SynText = SynText
  { _synTextContent  :: Text
  , _synTextPosition :: Int
  , _synTextEditMode :: Bool
  } deriving (Eq, Ord, Show)

makeLenses ''SynText

instance Semigroup SynText where
  syn1 <> syn2 = syn1
    & synTextContent  %~ mappend (syn2 ^. synTextContent)
    & synTextPosition %~ max     (syn2 ^. synTextPosition)
    & synTextEditMode %~ (&&)    (syn2 ^. synTextEditMode)

instance Monoid SynText where
  mempty = SynText "" 0 True

splitSynText :: SynText -> (Text, Text)
splitSynText syn = Text.splitAt (syn ^. synTextPosition) (syn ^. synTextContent)

insertSynText :: Text -> SynText -> SynText
insertSynText t syn =
  let (before, after) = splitSynText syn
  in syn & synTextContent .~ before <> t <> after

normalizeSynText :: SynText -> SynText
normalizeSynText syn = syn & synTextPosition %~ normalizePosition
  where
    normalizePosition :: Int -> Int
    normalizePosition = max 0 . min (views synTextContent Text.length syn)

instance UndoEq SynText where
  undoEq syn1 syn2
    | view synTextEditMode syn1 = True
    | view synTextEditMode syn2 = False
    | otherwise = on (==) (view synTextContent) syn1 syn2

instance SyntaxLayout SynText where
  layout syn = reader $ \lctx ->
    collageWithMargin (Margin 4 4 4 4) $
    textWithCursor
      (syn ^. synTextContent)
      (\Paths{..} -> \case
          _ | not (syn ^. synTextEditMode) -> Nothing
          _ | pathsSelection /= lctx ^. lctxPath -> Nothing
          CursorInvisible -> Nothing
          CursorVisible -> Just . fromIntegral $ syn ^. synTextPosition)

instance SyntaxReact SynText where
  react = do asum @[] handlers; modify normalizeSynText
    where
      handlers =
        [ handle_i
        , handleEscape
        , handleEnter
        , handleBackspace
        , handleDelete
        , handleArrowLeft
        , handleArrowRight
        , handleControl_v
        , handleLetter ]
      handle_i = do
        False <- use synTextEditMode
        KeyPress [] keyCode <- view rctxInputEvent
        guard (keyLetter 'i' keyCode)
        synTextEditMode .= True
      handleEscape = do
        KeyPress [] KeyCode.Escape <- view rctxInputEvent
        synTextEditMode .= False
      handleEnter = do
        KeyPress [] KeyCode.Enter <- view rctxInputEvent
        synTextEditMode .= False
      handleBackspace = do
        True <- use synTextEditMode
        KeyPress [] KeyCode.Backspace <- view rctxInputEvent
        True <- uses synTextPosition (>0)
        synTextPosition -= 1
        (before, after) <- gets splitSynText
        synTextContent .= before <> Text.drop 1 after
      handleDelete = do
        True <- use synTextEditMode
        KeyPress [] KeyCode.Delete <- view rctxInputEvent
        (before, after) <- gets splitSynText
        synTextContent .= before <> Text.drop 1 after
      handleArrowLeft = do
        True <- use synTextEditMode
        KeyPress [] KeyCode.ArrowLeft <- view rctxInputEvent
        synTextPosition -= 1
      handleArrowRight = do
        True <- use synTextEditMode
        KeyPress [] KeyCode.ArrowRight <- view rctxInputEvent
        synTextPosition += 1
      handleControl_v = do
        True <- use synTextEditMode
        KeyPress [Control] keyCode <- view rctxInputEvent
        guard (keyLetter 'v' keyCode)
        asyncReact <- view rctxAsyncReact
        liftIO $ do
          clipboard <- Gtk.clipboardGet Gtk.selectionClipboard
          Gtk.clipboardRequestText clipboard $ \case
            Nothing -> return ()
            Just str -> do
              asyncReact
                $ over synTextPosition (+length str)
                . insertSynText (Text.pack str)
      handleLetter = do
        True <- use synTextEditMode
        KeyPress _ keyCode <- view rctxInputEvent
        Just c <- pure (keyChar keyCode)
        modify (insertSynText (Text.singleton c))
        synTextPosition %= succ
