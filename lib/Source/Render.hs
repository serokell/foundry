module Source.Render where

import Data.Foldable
import Data.Functor.Identity
import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core
import Slay.Cairo.Render

import Source.Draw

render :: CollageRep (Draw a) -> Cairo.Render ()
render = traverse_ renderDrawElement . collageRepElements

renderDrawElement :: (Offset, Extents, Draw a) -> Cairo.Render ()
renderDrawElement (off, extents, d) = case d of
  DrawEmbed _ _ -> return ()
  DrawRect r -> renderElement runIdentity (off, extents, r)
  DrawText t -> renderElement runIdentity (off, extents, t)
