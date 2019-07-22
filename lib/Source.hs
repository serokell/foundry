{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Source
    ( runGUI,
      readTyEnv,
    ) where

import Control.Monad
import Control.Monad.State
import qualified Graphics.UI.Gtk as Gtk
import Data.IORef
import Control.Lens
import Data.Tuple
import Text.Megaparsec (errorBundlePretty)

import Slay.Core
import Source.Phaser
import Source.Input (InputEvent(..), Modifier(..))
import qualified Source.NewGen as NG
import qualified Sdam.Parser

runGUI :: NG.Plugin -> IO NG.EditorState -> IO ()
runGUI plugin initEditorState = do
  let pluginInfo = NG.mkPluginInfo plugin
  _ <- Gtk.initGUI
  esRef <- initEditorState >>= newIORef
  window <- createMainWindow pluginInfo esRef
  Gtk.widgetShowAll window
  Gtk.mainGUI

createMainWindow :: NG.PluginInfo -> IORef NG.EditorState -> IO Gtk.Window
createMainWindow pluginInfo esRef = do
  window <- Gtk.windowNew
  _ <- Gtk.on window Gtk.objectDestroy Gtk.mainQuit

  initialStackVis <-
    view NG.esStackVis <$> readIORef esRef

  canvas <- createMainCanvas

  -- TODO: PointerMotionHintMask; eventRequestMotions
  Gtk.widgetAddEvents canvas
      [ Gtk.PointerMotionMask
      , Gtk.ButtonPressMask
      ]

  layoutRef :: IORef (Collage NG.Draw)
    <- newIORef (error "layoutRef used before initialization")

  pointerRef :: IORef (Maybe Offset)
    <- newIORef Nothing

  cursorPhaser <- newPhaser 530000 NG.CursorVisible NG.blink $
    \_ -> Gtk.postGUIAsync (Gtk.widgetQueueDraw canvas)

  stackPhaser <- newPhaser 530000 initialStackVis (const NG.StackHidden) $
    \t -> Gtk.postGUIAsync $ do
      atomicRunStateIORef' esRef $ do
        NG.esStackVis .= t
      Gtk.widgetQueueDraw canvas

  let
    updateCanvas viewport = do
      es <- liftIO $ readIORef esRef
      let
        lctx =
          NG.LayoutCtx
            { NG._lctxPath = mempty @NG.PathBuilder,
              NG._lctxViewport = viewport,
              NG._lctxPrecBordersAlways = es ^. NG.esPrecBordersAlways,
              NG._lctxRecLayouts = pluginInfo ^. NG.pluginInfoRecLayouts,
              NG._lctxWritingDirection = es ^. NG.esWritingDirection }
        layout = NG.layoutEditorState lctx es
      liftIO $ writeIORef layoutRef layout
      cursorVisible <- liftIO $ phaserCurrent cursorPhaser
      let
        drawing = foldMapCollage NG.toDrawing offsetZero layout
        pathsCursor = NG.drawingFindPath drawing (es ^. NG.esPointer)
        pathsSelection = NG.selectionOfEditorState es
        paths = NG.Paths {NG.pathsCursor, NG.pathsSelection}
      NG.drawingRender drawing (NG.withDrawCtx paths cursorVisible)

    handleInputEvent inputEvent = do
      es <- readIORef esRef
      layout <- readIORef layoutRef
      let
        drawing = foldMapCollage NG.toDrawing offsetZero layout
        rctx =
          NG.ReactCtx
            { NG._rctxTyEnv = pluginInfo ^. NG.pluginInfoTyEnv,
              NG._rctxFindPath = NG.drawingFindPath drawing,
              NG._rctxNodeFactory = pluginInfo ^. NG.pluginInfoNodeFactory,
              NG._rctxDefaultNodes = pluginInfo ^. NG.pluginInfoDefaultNodes,
              NG._rctxAllowedFieldTypes = pluginInfo ^. NG.pluginInfoAllowedFieldTypes,
              NG._rctxRecMoveMaps = pluginInfo ^. NG.pluginInfoRecMoveMaps,
              NG._rctxWritingDirection = es ^. NG.esWritingDirection }
        mEs' = NG.reactEditorState inputEvent rctx es
      case mEs' of
        NG.UnknownEvent -> do
          print inputEvent
          return False
        NG.ReactOk es' -> do
          atomicWriteIORef esRef es'
          Gtk.widgetQueueDraw canvas
          phaserReset stackPhaser (es ^. NG.esStackVis)
          return True

  void $ Gtk.on canvas Gtk.draw $ do
    w <- liftIO $ Gtk.widgetGetAllocatedWidth  canvas
    h <- liftIO $ Gtk.widgetGetAllocatedHeight canvas
    let viewport = Extents (fromIntegral w) (fromIntegral h)
    updateCanvas viewport

  void $ Gtk.on canvas Gtk.keyPressEvent $ do
    modifier <- Gtk.eventModifier
    keyVal <- Gtk.eventKeyVal
    let
      event = KeyPress (modifier >>= gtkMod) keyVal
      gtkMod = \case
        Gtk.Control -> [Control]
        Gtk.Shift -> [Shift]
        Gtk.Alt -> [Alt]
        _ -> []
    liftIO $ do
      phaserReset cursorPhaser NG.CursorVisible
      handleInputEvent event

  void $ Gtk.on canvas Gtk.motionNotifyEvent $ do
    (x, y) <- Gtk.eventCoordinates
    let (x', y') = (round x, round y)
    liftIO $ atomicWriteIORef pointerRef (Just (Offset x' y'))
    let event = PointerMotion (fromInteger x') (fromInteger y')
    liftIO (handleInputEvent event)

  void $ Gtk.on canvas Gtk.buttonPressEvent $ do
    liftIO (handleInputEvent ButtonPress)

  Gtk.containerAdd window canvas
  Gtk.windowMaximize window
  return window

createMainCanvas :: IO Gtk.DrawingArea
createMainCanvas = do
  canvas <- Gtk.drawingAreaNew
  Gtk.set canvas
    [ Gtk.widgetExpand   Gtk.:= True
    , Gtk.widgetCanFocus Gtk.:= True
    , Gtk.widgetHasFocus Gtk.:= True
    ]
  return canvas


readTyEnv :: FilePath -> IO (Either String NG.Env)
readTyEnv path = do
  tyEnvDesc <- readFile path
  return $ over _Left errorBundlePretty $
    Sdam.Parser.parse
      Sdam.Parser.pEnv
      path
      tyEnvDesc

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)
