module Source.Render where

import Data.Foldable
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Slay.Core

import Source.Style
import Source.Draw

setSourceColor :: Color -> Cairo.Render ()
setSourceColor (RGB r g b) = Cairo.setSourceRGB r g b

render :: CollageRep (Draw a) -> Cairo.Render ()
render l = do
  Cairo.setAntialias Cairo.AntialiasNone
  renderCollage l

renderCollage :: CollageRep (Draw a) -> Cairo.Render ()
renderCollage = traverse_ renderElement . collageRepElements

renderElement :: (Offset, Extents, Draw a) -> Cairo.Render ()
renderElement (Offset left top, Extents width height, d) = case d of
  DrawEmbed _ _ -> return ()
  DrawPhantom _ -> return ()
  DrawOutline _ color -> do
    setSourceColor color
    Cairo.setLineWidth 1
    rectangle
    Cairo.stroke
  DrawBackground _ color -> do
    setSourceColor color
    rectangle
    Cairo.fill
  DrawText _ color pangoLayout -> do
    Cairo.moveTo (realToFrac left) (realToFrac top)
    setSourceColor color
    Pango.showLayout pangoLayout
  where
    rectangle = Cairo.rectangle
      (realToFrac left)
      (realToFrac top)
      (realToFrac width)
      (realToFrac height)
