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

data Draw a
  = DrawText (PangoText Identity)
  | DrawRect (PrimRect Identity)
  | DrawEmbed Extents a
  deriving Functor

instance p ~ Draw a => Inj p (Draw a)

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

-- FIXME: underflow due to Extents being unsigned
center :: s -/ Draw a => Extents -> Collage s -> Collage s
center (Extents vacantWidth vacantHeight) collage =
  let
    Extents width height = collageExtents collage
    excessWidth = max 0 (vacantWidth - width)
    excessHeight = max 0 (vacantHeight - height)
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
horizontalCenter = horizontal -- TODO
verticalCenter = vertical -- TODO

-- align :: Integral n => Op2 n -> Op2 n -> (Extents n -> Offset n) -> Op2 (CollageBuilderDraw n a)
-- align adj1 adj2 move c1 c2 =
--   let vacant = Point adj1 adj2 <*> B.getExtents c1 <*> B.getExtents c2
--   in B.overlay move (center vacant c1) (center vacant c2)

-- verticalCenter, horizontalCenter :: Integral n => OpN (CollageBuilderDraw n a)
-- verticalCenter   = foldr (align max (\_ _ -> 0) (set pointX 0)) mempty
-- horizontalCenter = foldr (align (\_ _ -> 0) max (set pointY 0)) mempty
