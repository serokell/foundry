{-# LANGUAGE FunctionalDependencies, DeriveFunctor, NamedFieldPuns, TypeApplications #-}
module Source.Draw
  ( module Source.Draw,
    module Slay.Core,
    module Slay.Combinators,
    module Slay.Cairo.Prim.Color,
    module Slay.Cairo.Prim.Rect,
    FontWeight(..), Font(..),
    module Inj
  ) where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.List.NonEmpty
import Control.Monad
import Data.Monoid
import qualified Graphics.Rendering.Cairo.Matrix as Cairo.Matrix

import Inj
import Inj.Base ()

import Slay.Core
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Cairo.Prim.PangoText
import Slay.Combinators
import Slay.Cairo.Render

data CursorBlink = CursorVisible | CursorInvisible

blink :: CursorBlink -> CursorBlink
blink = \case
  CursorVisible -> CursorInvisible
  CursorInvisible -> CursorVisible

data DrawCtx path a = DrawCtx (Maybe path -> CursorBlink -> a)
  deriving (Functor)

instance Inj p a => Inj p (DrawCtx path a) where
  inj p = DrawCtx (\_ _ -> inj p)

withDrawCtx :: Maybe path -> CursorBlink -> DrawCtx path a -> a
withDrawCtx mpath curBlink (DrawCtx f) = f mpath curBlink

data Draw path
  = DrawText (PangoText (DrawCtx path))
  | DrawRect (PrimRect (DrawCtx path))
  | DrawEmbed (PrimRect (DrawCtx path)) path

instance p ~ Draw a => Inj p (Draw a)

data Empty t = Empty

nothing :: Inj (Empty Maybe) a => a
nothing = inj (Empty @Maybe)

-- This overlapping instance should make it into inj-base in a better
-- form (no overlapping).

instance {-# OVERLAPPING #-} t ~ Maybe => Inj (Empty t) (Maybe a) where
  inj Empty = Nothing

lrtb :: Inj (LRTB p) a => p -> p -> p -> p -> a
lrtb left right top bottom = inj LRTB{left, right, top, bottom}

instance RenderElement (DrawCtx path) (Draw path) where
  renderElement getG (offset, extents, d) =
    case d of
      DrawEmbed r _ -> renderElement getG (offset, extents, r)
      DrawRect r -> renderElement getG (offset, extents, r)
      DrawText t -> renderElement getG (offset, extents, t)

dExtents :: Draw a -> Extents
dExtents = \case
  DrawText t -> ptextExtents t
  DrawRect r -> rectExtents r
  DrawEmbed r _ -> rectExtents r

phantom :: s -/ Draw a => Extents -> Collage s
phantom e = inj (DrawRect (rect nothing nothing e))

outline :: s -/ Draw a => Color -> Collage s -> Collage s
outline color c =
  c <> inj (DrawRect (rect (lrtb @Integer 1 1 1 1) (inj color) (collageExtents c)))

background :: s -/ Draw a => Color -> Collage s -> Collage s
background color c =
  inj (DrawRect (rect nothing (inj color) (collageExtents c))) <> c

textline :: s -/ Draw a => Color -> Font -> Text -> (CursorBlink -> Maybe Natural) -> Collage s
textline color font str cur = inj $ DrawText $ primTextPango Cairo.Matrix.identity $
  text font (inj color) str (DrawCtx (\_ -> cur))

line :: s -/ Draw a => Color -> Natural -> Collage s
line color w = inj (DrawRect (rect nothing (inj color) (Extents w 1)))

pad :: s -/ Draw a => LRTB Natural -> Collage s -> Collage s
pad padding = substrate padding (\e -> DrawRect $ rect nothing nothing e)

center :: s -/ Draw a => Extents -> Collage s -> Collage s
center (Extents vacantWidth vacantHeight) collage =
  let
    Extents width height = collageExtents collage
    (excessWidth1, excessWidth2) = integralDistribExcess vacantWidth width
    (excessHeight1, excessHeight2) = integralDistribExcess vacantHeight height
    padding =
      LRTB
        { left = excessWidth1,
          right = excessWidth2,
          top = excessHeight1,
          bottom = excessHeight2 }
  in
    pad padding collage

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
  (Natural -> Natural -> Natural) ->
  (Natural -> Natural -> Natural) ->
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

activate ::
  Offset ->
  NonEmpty (Offset, Extents, Draw path) ->
  Maybe (Offset, Extents, path)
activate o c =
  getFirst $ foldMap (First . check) c
  where
    check (o', e, d) = do
      DrawEmbed _ p <- Just d
      guard $ insideBox (o', e) o
      Just (o', e, p)

active :: (s -/ Draw path, Eq path) => Color -> path -> Collage s -> Collage s
active color p c = c <> inj activeZone
  where
    mkColor (Just path) | path == p = Just color
    mkColor _ = Nothing
    outlineRect =
      rect (lrtb @Integer 1 1 1 1) (DrawCtx $ \mpath _ -> mkColor mpath) (collageExtents c)
    activeZone = DrawEmbed outlineRect p
