module Source
    ( runGUI
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import qualified Graphics.UI.Gtk as Gtk
import Data.IORef

import Slay.Core
import Slay.Cairo.Render
import qualified Source.Syntax as Syn
import Source.Draw
import Source.Input (InputEvent(..), Modifier(..))

runGUI
  :: Syn.SyntaxBlank syn
  => Syn.SyntaxLayout la Syn.Viewport syn
  => Syn.SyntaxReact () la syn
  => IO syn
runGUI = do
  _ <- Gtk.initGUI
  synRef <- Syn.blank >>= newIORef
  createMainWindow synRef >>= Gtk.widgetShowAll
  Gtk.mainGUI
  readIORef synRef

createMainWindow
  :: forall la syn.
     Syn.SyntaxBlank syn
  => Syn.SyntaxLayout la Syn.Viewport syn
  => Syn.SyntaxReact () la syn
  => IORef syn
  -> IO Gtk.Window
createMainWindow synRef = do
  window <- Gtk.windowNew
  _ <- Gtk.on window Gtk.objectDestroy Gtk.mainQuit

  canvas <- createMainCanvas

  -- TODO: PointerMotionHintMask; eventRequestMotions
  Gtk.widgetAddEvents canvas
      [ Gtk.PointerMotionMask
      , Gtk.ButtonPressMask
      ]

  layoutRef :: IORef (Syn.LayoutDraw la)
    <- newIORef (error "layoutRef used before initialization")

  let

    asyncReact f = do
      atomicModifyIORef' synRef (\syn -> (f syn, ()))
      Gtk.postGUIAsync (Gtk.widgetQueueDraw canvas)

    updateCanvas viewport = do
      syn <- liftIO $ readIORef synRef
      let layout = Syn.LayoutDraw (\ElementRefl -> runReader (Syn.layout syn) viewport)
      liftIO $ writeIORef layoutRef layout
      renderElements runIdentity . collageRepElements $
        Syn.runLayoutDraw (\d -> (dExtents d, d)) layout

    handleInputEvent inputEvent = do
      syn <- liftIO $ readIORef synRef
      layout <- liftIO $ readIORef layoutRef
      let reactCtx = Syn.ReactCtx asyncReact layout inputEvent ()
      msyn'
        <- runMaybeT
         . flip execStateT syn
         . flip runReaderT reactCtx
         $ Syn.react
      case msyn' of
        Nothing -> return False
        Just syn' -> do
          atomicWriteIORef synRef syn'
          Gtk.widgetQueueDraw canvas
          return True

  void $ Gtk.on canvas Gtk.draw $ do
    w <- liftIO $ Gtk.widgetGetAllocatedWidth  canvas
    h <- liftIO $ Gtk.widgetGetAllocatedHeight canvas
    let viewport = Syn.Viewport (Extents (fromIntegral w) (fromIntegral h))
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
    liftIO (handleInputEvent event)

  void $ Gtk.on canvas Gtk.motionNotifyEvent $ do
    (x, y) <- Gtk.eventCoordinates
    let event = PointerMotion (round x) (round y)
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
