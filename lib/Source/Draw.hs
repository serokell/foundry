{-# LANGUAGE FunctionalDependencies #-}
module Source.Draw
  ( module Source.Collage
  , module Source.Draw
  ) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Rendering.Pango as Pango
import System.IO.Unsafe (unsafePerformIO)

import Source.Style
import Source.Collage

import Source.Collage.Builder (CollageBuilder, collageBuilder1)
import qualified Source.Collage.Builder as B

data Draw a
  = DrawText Color Pango.PangoLayout
  | DrawPhantom
  | DrawOutline Color
  | DrawBackground Color
  | DrawEmbed a

type CollageBuilderDraw n a = CollageBuilder n (Draw a)

phantom :: (Num n, Ord n) => Extents n -> CollageBuilderDraw n a
phantom e = collageBuilder1 e DrawPhantom

extend :: (Num n, Ord n) => Extents n -> Op1 (CollageBuilderDraw n a)
extend o c = phantom (pointAdd o (B.getExtents c)) <> c

outline :: (Num n, Ord n) => Color -> Op1 (CollageBuilderDraw n a)
outline color c =
  c <> collageBuilder1 (B.getExtents c) (DrawOutline color)

background :: (Num n, Ord n) => Color -> Op1 (CollageBuilderDraw n a)
background color c =
  collageBuilder1 (B.getExtents c) (DrawBackground color) <> c

textline :: Integral n => Font -> Text -> CollageBuilderDraw n a
textline font text =
  let (e, a) = drawText font text
  in collageBuilder1 e a

drawText :: Integral n => Font -> Text -> (Extents n, Draw a)
drawText font text = unsafePerformIO $ do
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
  return
    ( Point (ceiling w) (ceiling h)
    , DrawText (fontColor font) pangoLayout )

line :: (Num n, Ord n) => Color -> n -> CollageBuilderDraw n a
line color w = collageBuilder1 (Point w 1) (DrawBackground color)

pad :: (Num n, Ord n) => Offset n -> Offset n -> Op1 (CollageBuilderDraw n a)
pad o1 o2 = B.offset o1 . extend o2

center :: Integral n => Extents n -> Op1 (CollageBuilderDraw n a)
center (Point vacantWidth vacantHeight) collage =
  let
    Point width height = B.getExtents collage
    (excessWidth1,  excessWidth2)  = getExcess vacantWidth  width
    (excessHeight1, excessHeight2) = getExcess vacantHeight height
  in
    collage & pad
      (Point excessWidth1 excessHeight1)
      (Point excessWidth2 excessHeight2)

align :: Integral n => Op2 n -> Op2 n -> (Extents n -> Offset n) -> Op2 (CollageBuilderDraw n a)
align adj1 adj2 move c1 c2 =
  let vacant = Point adj1 adj2 <*> B.getExtents c1 <*> B.getExtents c2
  in B.overlay move (center vacant c1) (center vacant c2)

verticalCenter, horizontalCenter :: Integral n => OpN (CollageBuilderDraw n a)
verticalCenter   = foldr (align max (\_ _ -> 0) (set pointX 0)) mempty
horizontalCenter = foldr (align (\_ _ -> 0) max (set pointY 0)) mempty
