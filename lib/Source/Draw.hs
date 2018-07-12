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

instance c ~ DrawCtx path => Inj (PangoText c) (Draw path) where
  inj = DrawText

instance c ~ DrawCtx path => Inj (PrimRect c) (Draw path) where
  inj = DrawRect

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

textline :: s -/ Draw a => Color -> Font -> Text -> (CursorBlink -> Maybe Natural) -> Collage s
textline color font str cur = inj $ primTextPango Cairo.Matrix.identity $
  text font (inj color) str (DrawCtx (\_ -> cur))

line :: s -/ Draw a => Color -> Natural -> Collage s
line color w = rect nothing (inj color) (Extents w 1)

pad :: s -/ Draw a => LRTB Natural -> Collage s -> Collage s
pad padding = substrate padding (\e -> rect (inj padding) nothing e)

centerOf :: s -/ Draw a => Extents -> Collage s -> LRTB Natural
centerOf (Extents vacantWidth vacantHeight) collage =
  let
    Extents width height = collageExtents collage
    (excessWidth1, excessWidth2) = integralDistribExcess vacantWidth width
    (excessHeight1, excessHeight2) = integralDistribExcess vacantHeight height
  in
    LRTB
      { left = excessWidth1,
        right = excessWidth2,
        top = excessHeight1,
        bottom = excessHeight2 }

horizontal, vertical, horizontalCenter :: NonEmpty (Collage s) -> Collage s
horizontal = foldr1 @NonEmpty horizTop
vertical = foldr1 @NonEmpty vertLeft
horizontalCenter = foldr1 @NonEmpty horizCenter

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
active color p c = c <> collageSingleton activeZone
  where
    mkColor (Just path) | path == p = Just color
    mkColor _ = Nothing
    outlineRect =
      rect (lrtb @Natural 1 1 1 1) (DrawCtx $ \mpath _ -> mkColor mpath) (collageExtents c)
    activeZone = DrawEmbed outlineRect p
