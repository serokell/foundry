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

runGUI :: NG.Plugin -> NG.EditorState -> IO ()
runGUI plugin editorState = do
  let pluginInfo = NG.mkPluginInfo plugin
  _ <- Gtk.initGUI
  esRef <- newIORef editorState
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

  cursorPhaser <- newPhaser 530000 NG.CursorVisible NG.blink $
    \_ -> Gtk.postGUIAsync (Gtk.widgetQueueDraw canvas)

  stackPhaser <- newPhaser 530000 initialStackVis (const NG.StackHidden) $
    \t -> Gtk.postGUIAsync $ do
      atomicRunStateIORef' esRef $ do
        NG.esStackVis .= t
      Gtk.widgetQueueDraw canvas

  let
    updateCanvas viewport = do
      es <- liftIO $
        atomicRunStateIORef' esRef $ do
          modify (NG.redrawUI pluginInfo viewport)
          get
      cursorBlink <- liftIO $ phaserCurrent cursorPhaser
      (es ^. NG.esRenderUI) cursorBlink

    handleInputEvent inputEvent = do
      es <- readIORef esRef
      let mEs' = NG.reactEditorState pluginInfo inputEvent es
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
