module Source.Render where

import Control.Lens (view)
import Data.Foldable
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import Source.Style
import Source.Draw

setSourceColor :: Color -> Cairo.Render ()
setSourceColor (RGB r g b) = Cairo.setSourceRGB r g b

render :: Real n => Collage n (Draw a) -> Cairo.Render ()
render l = do
  Cairo.setAntialias Cairo.AntialiasNone
  renderCollage l

renderCollage :: Real n => Collage n (Draw a) -> Cairo.Render ()
renderCollage = traverse_ renderElement . getCollage

renderElement :: Real n => Element n (Draw a) -> Cairo.Render ()
renderElement e = case view elementObject e of
  DrawEmbed _ -> return ()
  DrawPhantom -> return ()
  DrawOutline color -> do
    setSourceColor color
    Cairo.setLineWidth 1
    rectangle
    Cairo.stroke
  DrawBackground color -> do
    setSourceColor color
    rectangle
    Cairo.fill
  DrawText color pangoLayout -> do
    Cairo.moveTo (realToFrac left) (realToFrac top)
    setSourceColor color
    Pango.showLayout pangoLayout
  where
    Point left top = view elementOffset e
    Point width height = view elementExtents e
    rectangle = Cairo.rectangle
      (realToFrac left)
      (realToFrac top)
      (realToFrac width)
      (realToFrac height)
