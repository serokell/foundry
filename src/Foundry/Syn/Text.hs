{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Text where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import Data.Monoid

import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import qualified Graphics.UI.Gtk as Gtk

import Source.Syntax
import Source.Draw
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Common

data SynText = SynText
  { _synTextContent  :: Text
  , _synTextPosition :: Int
  , _synTextEditMode :: Bool
  } deriving (Eq, Ord, Show)

makeLenses ''SynText

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
instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynText where

  layout lctx syn
    | lctx ^. lctxSelected, syn ^. synTextEditMode =
        let
          (t1, t2) = Text.splitAt
            (syn ^. synTextPosition)
            (syn ^. synTextContent)
        in horizontal [text t1, punct "|", text t2]
    | otherwise = text (syn ^. synTextContent)

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) SynText where

  react asyncReact _oldLayout inputEvent = do
    asum handlers
    modify normalizeSynText
    where
      handlers :: [StateT SynText (ExceptT () IO) ()]
      handlers =
        [ handle_i
        , handleEscape
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
