module Source.Layout.Cairo.Prim.Circle (circle) where

import Numeric.Natural
import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Source.Layout.Inj
import Source.Layout.NonNegative
import Source.Layout.Core

import Source.Layout.Cairo.Prim.Color
import Source.Layout.Cairo.Element

circle ::
  forall g a.
  Inj (CairoElement g) a =>
  g Color ->
  g (Maybe (NonNegative Double)) ->
  Natural ->
  a
circle gcolor gmthickness diameter =
  inj CairoElement
    { cairoElementExtents = extents,
      cairoElementBaseline = NoBaseline,
      cairoElementRender = render }
  where
    extents = Extents diameter diameter

    render :: Offset -> CairoRender g
    render offset = CairoRender $ \getG -> do
      let (Offset (fromIntegral -> x) (fromIntegral -> y)) = offset
      setSourceColor $ getG gcolor
      let r = fromIntegral diameter / 2
      for_ (getG gmthickness) $ \thck -> do
        Cairo.setFillRule Cairo.FillRuleEvenOdd
        Cairo.arc (r + x) (r + y) (r - getNonNegative thck) 0 (2 * pi)
      Cairo.arc (r + x) (r + y) r 0 (2 * pi)
      Cairo.fill
