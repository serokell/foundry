{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Text where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import Data.Function
import Data.Monoid

import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import qualified Graphics.UI.Gtk as Gtk

import Source.Collage.Builder (horizontal)
import Source.Syntax
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Common
import Foundry.Syn.TH

data TEXT

data instance SYN TEXT = SynText
  { _synTextContent  :: Text
  , _synTextPosition :: Int
  , _synTextEditMode :: Bool
  } deriving (Eq, Ord, Show)

makeLensesDataInst ''SYN ''TEXT

instance Monoid (SYN TEXT) where
  mempty = SynText "" 0 True
  mappend syn1 syn2 = syn1
    & synTextContent  %~ mappend (syn2 ^. synTextContent)
    & synTextPosition %~ max     (syn2 ^. synTextPosition)
    & synTextEditMode %~ (&&)    (syn2 ^. synTextEditMode)

splitSynText :: SYN TEXT -> (Text, Text)
splitSynText syn = Text.splitAt (syn ^. synTextPosition) (syn ^. synTextContent)

insertSynText :: Text -> SYN TEXT -> SYN TEXT
insertSynText t syn =
  let (before, after) = splitSynText syn
  in syn & synTextContent .~ before <> t <> after

normalizeSynText :: SYN TEXT -> SYN TEXT
normalizeSynText syn = syn & synTextPosition %~ normalizePosition
  where
    normalizePosition :: Int -> Int
    normalizePosition = max 0 . min (views synTextContent Text.length syn)

instance UndoEq (SYN TEXT) where
  undoEq syn1 syn2
    | view synTextEditMode syn1 = True
    | view synTextEditMode syn2 = False
    | otherwise = on (==) (view synTextContent) syn1 syn2

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m ActiveZone LayoutCtx (SYN TEXT) where

  layout lctx syn
    | lctx ^. lctxSelected, syn ^. synTextEditMode =
        let
          (t1, t2) = Text.splitAt
            (syn ^. synTextPosition)
            (syn ^. synTextContent)
        in horizontal [text t1, punct "|", text t2]
    | otherwise = text (syn ^. synTextContent)

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m ActiveZone (SYN TEXT) where

  react asyncReact _oldLayout inputEvent = do
    asum handlers
    modify normalizeSynText
    where
      handlers :: [StateT (SYN TEXT) (ExceptT () IO) ()]
      handlers =
        [ handle_i
        , handleEscape
        , handleEnter
        , handleBackspace
        , handleDelete
        , handleArrowLeft
        , handleArrowRight
        , handleControl_v
        , handleLetter
        ]
      handle_i = do
        guard =<< uses synTextEditMode not
        case inputEvent of
          KeyPress [] keyCode | keyLetter 'i' keyCode
            -> synTextEditMode .= True
          _ -> mzero
      handleEscape = do
        case inputEvent of
          KeyPress [] KeyCode.Escape
            -> synTextEditMode .= False
          _ -> mzero
      handleEnter = do
        case inputEvent of
          KeyPress [] KeyCode.Enter
            -> synTextEditMode .= False
          _ -> mzero
      handleBackspace = do
        guard =<< use synTextEditMode
        case inputEvent of
          KeyPress [] KeyCode.Backspace -> return ()
          _ -> mzero
        guard =<< uses synTextPosition (>0)
        synTextPosition -= 1
        (before, after) <- gets splitSynText
        synTextContent .= before <> Text.drop 1 after
      handleDelete = do
        guard =<< use synTextEditMode
        case inputEvent of
          KeyPress [] KeyCode.Delete -> return ()
          _ -> mzero
        (before, after) <- gets splitSynText
        synTextContent .= before <> Text.drop 1 after
      handleArrowLeft = do
        guard =<< use synTextEditMode
        case inputEvent of
          KeyPress [] KeyCode.ArrowLeft -> return ()
          _ -> mzero
        synTextPosition -= 1
      handleArrowRight = do
        guard =<< use synTextEditMode
        case inputEvent of
          KeyPress [] KeyCode.ArrowRight -> return ()
          _ -> mzero
        synTextPosition += 1
      handleControl_v = do
        guard =<< use synTextEditMode
        case inputEvent of
          KeyPress [Control] keyCode | keyLetter 'v' keyCode -> do
            clipboard <- liftIO $ Gtk.clipboardGet Gtk.selectionClipboard
            liftIO $ Gtk.clipboardRequestText clipboard $ \case
              Nothing -> return ()
              Just str -> asyncReact
                $ over synTextPosition (+length str)
                . insertSynText (Text.pack str)
          _ -> mzero
      handleLetter = do
        guard =<< use synTextEditMode
        case inputEvent of
          KeyPress _ keyCode | Just c <- keyChar keyCode
            -> modify (insertSynText (Text.singleton c))
            >> synTextPosition %= succ
          _ -> mzero
