module Source.Layout.Cairo.Prim.Rect (rect) where

import Data.Foldable (for_)

import qualified Graphics.Rendering.Cairo as Cairo

import Source.Layout.Inj
import Source.Layout.NonNegative
import Source.Layout.Core

import Source.Layout.Cairo.Prim.Color
import Source.Layout.Cairo.Element

rect ::
  forall g a.
  Inj (CairoElement g) a =>
  g (Maybe (LRTB (NonNegative Double))) ->
  g (Maybe Color) ->
  Extents ->
  a
rect gmthickness gmcolor extents =
  inj CairoElement
    { cairoElementExtents = extents,
      cairoElementBaseline = NoBaseline,
      cairoElementRender = render }
  where
    render :: Offset -> CairoRender g
    render (Offset x y) = CairoRender $ \getG -> do
      let Extents w h = extents
      for_ (getG gmcolor) $ \color -> do
        setSourceColor color
        for_ (getG gmthickness) $ \thck -> do
          Cairo.setFillRule Cairo.FillRuleEvenOdd
          Cairo.rectangle
            (fromIntegral x + getNonNegative (left thck))
            (fromIntegral y + getNonNegative (top thck))
            (fromIntegral w - getNonNegative (left thck + right thck))
            (fromIntegral h - getNonNegative (top thck + bottom thck))
        Cairo.rectangle
          (fromIntegral x)
          (fromIntegral y)
          (fromIntegral w)
          (fromIntegral h)
        Cairo.fill
