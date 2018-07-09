{-# LANGUAGE FunctionalDependencies, DeriveFunctor #-}
module Source.Draw
  ( module Source.Draw,
    module Slay.Core,
    module Slay.Combinators,
    module Slay.Cairo.Prim.Color,
    module Slay.Cairo.Prim.Rect,
    FontWeight(..), Font(..),
    module Inj
  ) where

import Control.Lens
import Data.Text (Text)
import Numeric.Natural (Natural)
import qualified Graphics.Rendering.Cairo.Matrix as Cairo.Matrix

import Inj

import Slay.Core
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Cairo.Prim.PangoText
import Slay.Combinators
import Slay.Cairo.Render

data Draw a
  = DrawText (PangoText Identity)
  | DrawRect (PrimRect Identity)
  | DrawEmbed Extents a
  deriving Functor

instance p ~ Draw a => Inj p (Draw a)

instance RenderElement Identity (Draw a) where
  renderElement getG (offset, extents, d) =
    case d of
      DrawEmbed _ _ -> return ()
      DrawRect r -> renderElement getG (offset, extents, r)
      DrawText t -> renderElement getG (offset, extents, t)

dExtents :: Draw a -> Extents
dExtents = \case
  DrawText t -> ptextExtents t
  DrawRect r -> rectExtents r
  DrawEmbed e _ -> e

phantom :: s -/ Draw a => Extents -> Collage s
phantom e = inj (DrawRect (rect (Identity Nothing) (Identity Nothing) e))

outline :: s -/ Draw a => Color -> Collage s -> Collage s
outline color c =
  c <> inj (DrawRect (rect (Identity (Just (LRTB 1 1 1 1))) (Identity (Just color)) (collageExtents c)))

background :: s -/ Draw a => Color -> Collage s -> Collage s
background color c =
  inj (DrawRect (rect (Identity Nothing) (Identity (Just color)) (collageExtents c))) <> c

textline :: s -/ Draw a => Color -> Font -> Text -> Maybe Natural -> Collage s
textline color font str mpos = inj $ DrawText $ primTextPango Cairo.Matrix.identity $
  text font (Identity color) str (Identity mpos)

line :: s -/ Draw a => Color -> Unsigned -> Collage s
line color w = inj (DrawRect (rect (Identity Nothing) (Identity (Just color)) (Extents w 1)))

pad :: s -/ Draw a => LRTB Unsigned -> Collage s -> Collage s
pad lrtb = substrate lrtb (\e -> DrawRect $ rect (Identity Nothing) (Identity Nothing) e)

center :: s -/ Draw a => Extents -> Collage s -> Collage s
center (Extents vacantWidth vacantHeight) collage =
  let
    Extents width height = collageExtents collage
    excessWidth =
      if vacantWidth >= width then vacantWidth - width else 0
    excessHeight =
      if vacantHeight >= height then vacantHeight - height else 0
    excessWidth1 = fromInteger (ceil (excessWidth/2))
    excessWidth2 = excessWidth - excessWidth1
    excessHeight1 = fromInteger (ceil (excessHeight/2))
    excessHeight2 = excessHeight - excessHeight1
    lrtb =
      LRTB
        { left = excessWidth1,
          right = excessWidth2,
          top = excessHeight1,
          bottom = excessHeight2 }
  in
    pad lrtb collage

horizontal :: s -/ Draw a => [Collage s] -> Collage s
horizontal [] = phantom (Extents 0 0)
horizontal xs = foldr1 horizTop xs

vertical :: s -/ Draw a => [Collage s] -> Collage s
vertical [] = phantom (Extents 0 0)
vertical xs = foldr1 vertLeft xs

horizontalCenter, verticalCenter :: s -/ Draw a => [Collage s] -> Collage s
verticalCenter [] = phantom (Extents 0 0)
verticalCenter xs =
  foldr1 (align max (\_ _ -> 0) (\e -> (extentsOffset e) { offsetX = 0 })) xs
horizontalCenter [] = phantom (Extents 0 0)
horizontalCenter xs =
  foldr1 (align (\_ _ -> 0) max (\e -> (extentsOffset e) { offsetY = 0 })) xs

align ::
  s -/ Draw a =>
  (Unsigned -> Unsigned -> Unsigned) ->
  (Unsigned -> Unsigned -> Unsigned) ->
  (Extents -> Offset) ->
  (Collage s -> Collage s -> Collage s)
align adj1 adj2 move c1 c2 =
  let
    e1 = collageExtents c1
    e2 = collageExtents c2
    vacant =
      Extents
        (extentsW e1 `adj1` extentsW e2)
        (extentsH e1 `adj2` extentsH e2)
  in
    collageCompose (move e1) (center vacant c1) (center vacant c2)
