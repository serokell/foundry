{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Source
  ( runSource,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Tuple
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import Sdam.Parser (ParsedValue)
import Slay.Core
import Source.Input (InputEvent (..), Modifier (..))
import qualified Source.NewGen as NG
import Source.Phaser

runSource :: NG.Plugin -> Maybe ParsedValue -> IO ()
runSource plugin mParsedValue = do
  let pluginInfo = NG.mkPluginInfo plugin
  _ <- Gtk.initGUI
  esRef <- newIORef $
    case mParsedValue of
      Nothing -> NG.initEditorState
      Just a ->
        let expr = NG.fromParsedValue a
         in NG.initEditorState {NG._esExpr = expr}
  window <- createMainWindow pluginInfo esRef
  Gtk.widgetShowAll window
  Gtk.mainGUI

createMainWindow :: NG.PluginInfo -> IORef NG.EditorState -> IO Gtk.Window
createMainWindow pluginInfo esRef = do
  window <- Gtk.windowNew
  Gtk.widgetSetAppPaintable window True
  _ <- Gtk.on window Gtk.objectDestroy Gtk.mainQuit
  -- TODO: PointerMotionHintMask; eventRequestMotions
  Gtk.widgetAddEvents
    window
    [ Gtk.PointerMotionMask,
      Gtk.ButtonPressMask
    ]
  cursorPhaser <- newPhaser 530000 NG.CursorVisible NG.blink $
    \_ -> Gtk.postGUIAsync (Gtk.widgetQueueDraw window)
  stackPhaser <- newPhaser 530000 () id $
    \_ -> Gtk.postGUIAsync $ do
      atomicRunStateIORef' esRef $ do
        NG.esMode %= NG.quitStackMode
      Gtk.widgetQueueDraw window
  let updateCanvas viewport = do
        es <- liftIO
          $ atomicRunStateIORef' esRef
          $ do
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
            Gtk.widgetQueueDraw window
            phaserReset stackPhaser ()
            return True
  void $ Gtk.on window Gtk.draw $ do
    (x1, y1, x2, y2) <- Cairo.clipExtents
    let (w, h) = (x2 - x1, y2 - y1)
    let viewport = Extents (floor w) (floor h)
    updateCanvas viewport
  void $ Gtk.on window Gtk.keyPressEvent $ do
    modifier <- Gtk.eventModifier
    keyVal <- Gtk.eventKeyVal
    let event = KeyPress (modifier >>= gtkMod) keyVal
    liftIO $ do
      phaserReset cursorPhaser NG.CursorVisible
      handleInputEvent event
  void $ Gtk.on window Gtk.motionNotifyEvent $ do
    (x, y) <- Gtk.eventCoordinates
    let (x', y') = (round x, round y)
    let event = PointerMotion (fromInteger x') (fromInteger y')
    liftIO (handleInputEvent event)
  void $ Gtk.on window Gtk.buttonPressEvent $ do
    liftIO (handleInputEvent ButtonPress)
  Gtk.windowMaximize window
  return window

gtkMod :: Gtk.Modifier -> [Modifier]
gtkMod = \case
  Gtk.Control -> [Control]
  Gtk.Shift -> [Shift]
  Gtk.Alt -> [Alt]
  _ -> []

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)
