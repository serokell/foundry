{-# LANGUAGE FunctionalDependencies, DeriveFunctor #-}
module Source.Draw
  ( module Source.Draw,
    module Slay.Core,
    module Slay.Combinators,
    module Inj
  ) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Rendering.Pango as Pango
import System.IO.Unsafe (unsafePerformIO)

import Inj

import Slay.Core
import Slay.Combinators

import Source.Style

data Draw a
  = DrawText Extents Color Pango.PangoLayout
  | DrawPhantom Extents
  | DrawOutline Extents Color
  | DrawBackground Extents Color
  | DrawEmbed Extents a
  deriving Functor

instance p ~ Draw a => Inj p (Draw a)

dExtents :: Draw a -> Extents
dExtents = \case
  DrawText e _ _ -> e
  DrawPhantom e -> e
  DrawOutline e _ -> e
  DrawBackground e _ -> e
  DrawEmbed e _ -> e

phantom :: s -/ Draw a => Extents -> Collage s
phantom e = inj (DrawPhantom e)

offset :: s -/ Draw a => Extents -> Collage s -> Collage s
offset e c = collageCompose (extentsOffset e) (phantom (Extents 0 0)) c

extend :: s -/ Draw a => Extents -> Collage s -> Collage s
extend e c = collageCompose (extentsOffset (collageExtents c)) c (phantom e)

outline :: s -/ Draw a => Color -> Collage s -> Collage s
outline color c =
  c <> inj (DrawOutline (collageExtents c) color)

background :: s -/ Draw a => Color -> Collage s -> Collage s
background color c =
  inj (DrawBackground (collageExtents c) color) <> c

textline :: s -/ Draw a => Font -> Text -> Collage s
textline font text = unsafePerformIO $ do
  cairoContext <- Pango.cairoCreateContext Nothing
  pangoFont <- Pango.fontDescriptionNew
  pangoFont `Pango.fontDescriptionSetFamily` Text.unpack (fontFamily font)
  pangoFont `Pango.fontDescriptionSetSize` fontSize font
  pangoFont `Pango.fontDescriptionSetWeight` (case fontWeight font of
      FontWeightNormal -> Pango.WeightNormal
      FontWeightBold -> Pango.WeightBold)
  pangoLayout <- Pango.layoutEmpty cairoContext
  pangoLayout `Pango.layoutSetText` Text.unpack text
  pangoLayout `Pango.layoutSetFontDescription` Just pangoFont
  -- TODO: handle x y
  (_, Pango.PangoRectangle _x _y w h) <- Pango.layoutGetExtents pangoLayout
  let e = Extents (fromInteger $ ceiling w) (fromInteger $ ceiling h)
  return . inj $ DrawText e (fontColor font) pangoLayout

line :: s -/ Draw a => Color -> Unsigned -> Collage s
line color w = inj (DrawBackground (Extents w 1) color)

pad :: s -/ Draw a => Extents -> Extents -> Collage s -> Collage s
pad o1 o2 = offset o1 . extend o2

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
  in
    collage & pad
      (Extents excessWidth1 excessHeight1)
      (Extents excessWidth2 excessHeight2)

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
