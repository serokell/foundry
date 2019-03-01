module Source
    ( runGUI
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.UI.Gtk as Gtk
import Data.IORef
import Control.Lens ((^.))

import Slay.Core
import Slay.Cairo.Render
import Source.Phaser
import Source.Input (InputEvent(..), Modifier(..))
import qualified Source.NewGen as NG

runGUI :: NG.Plugin -> IO NG.EditorState -> IO ()
runGUI plugin initEditorState = do
  _ <- Gtk.initGUI
  esRef <- initEditorState >>= newIORef
  window <- createMainWindow plugin esRef
  Gtk.widgetShowAll window
  Gtk.mainGUI

createMainWindow :: NG.Plugin -> IORef NG.EditorState -> IO Gtk.Window
createMainWindow plugin esRef = do
  window <- Gtk.windowNew
  _ <- Gtk.on window Gtk.objectDestroy Gtk.mainQuit

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

  cursorPhaser <- newPhaser 530000 NG.CursorVisible NG.blink
    (\_ -> Gtk.postGUIAsync (Gtk.widgetQueueDraw canvas))

  let

    updateCanvas viewport = do
      es <- liftIO $ readIORef esRef
      let
        lctx =
          NG.LayoutCtx
            { _lctxPath = NG.emptyPath,
              _lctxViewport = viewport,
              _lctxRecLayouts = plugin ^. NG.pluginRecLayouts }
        layout = NG.layoutEditorState lctx es
      liftIO $ writeIORef layoutRef layout
      cursorVisible <- liftIO $ phaserCurrent cursorPhaser
      let
        elements = collageElements offsetZero layout
        toCairoElements = (fmap.fmap) NG.toCairoElementDraw
        pathsCursor = NG.findPath (es ^. NG.esPointer) elements
        pathsSelection = NG.selectionPathEditorState es
      renderElements
        (NG.withDrawCtx NG.Paths{..} cursorVisible)
        (toCairoElements elements)

    handleInputEvent inputEvent = do
      es <- readIORef esRef
      layout <- readIORef layoutRef
      let
        tyEnv = plugin ^. NG.pluginTyEnv
        recLayouts = plugin ^. NG.pluginRecLayouts
        recMoveMaps = NG.mkRecMoveMaps tyEnv recLayouts
        rctx =
          NG.ReactCtx
            { _rctxLastLayout = layout,
              _rctxInputEvent = inputEvent,
              _rctxNodeFactory = plugin ^. NG.pluginNodeFactory,
              _rctxDefaultValues = NG.mkDefaultValues tyEnv recMoveMaps,
              _rctxAllowedFieldTypes = NG.mkAllowedFieldTypes tyEnv,
              _rctxRecMoveMaps = recMoveMaps }
      mEs' <- NG.reactEditorState rctx es
      case mEs' of
        Nothing -> do
          print inputEvent
          return False
        Just es' -> do
          atomicWriteIORef esRef es'
          Gtk.widgetQueueDraw canvas
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
