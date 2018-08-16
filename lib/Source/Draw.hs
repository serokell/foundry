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

import Inj
import Inj.Base ()

import Slay.Core
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Combinators
import Slay.Cairo.Element

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
  = DrawCairoElement (CairoElement (DrawCtx path))
  | DrawEmbed (CairoElement (DrawCtx path)) path

instance HasExtents (Draw path) where
  extentsOf = extentsOf . toCairoElementDraw

toCairoElementDraw :: Draw path -> CairoElement (DrawCtx path)
toCairoElementDraw = \case
  DrawCairoElement ce -> ce
  DrawEmbed ce _ -> ce

instance g ~ DrawCtx path => Inj (CairoElement g) (Draw path) where
  inj = DrawCairoElement

data Empty t = Empty

nothing :: Inj (Empty Maybe) a => a
nothing = inj (Empty @Maybe)

-- This overlapping instance should make it into inj-base in a better
-- form (no overlapping).

instance {-# OVERLAPPING #-} t ~ Maybe => Inj (Empty t) (Maybe a) where
  inj Empty = Nothing

lrtb :: Inj (LRTB p) a => p -> p -> p -> p -> a
lrtb left right top bottom = inj LRTB{left, right, top, bottom}

textline :: Color -> Font -> Text -> (CursorBlink -> Maybe Natural) -> Collage (Draw a)
textline color font str cur = text font (inj color) str (DrawCtx (\_ -> cur))

line :: Color -> Natural -> Collage (Draw a)
line color w = rect nothing (inj color) (Extents w 1)

pad :: LRTB Natural -> Collage (Draw a) -> Collage (Draw a)
pad padding = substrate padding (\e -> rect (inj padding) nothing e)

centerOf :: Extents -> Collage (Draw a) -> LRTB Natural
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

horizontal, vertical, horizontalCenter :: NonEmpty (Collage (Draw a)) -> Collage (Draw a)
horizontal = foldr1 @NonEmpty horizTop
vertical = foldr1 @NonEmpty vertLeft
horizontalCenter = foldr1 @NonEmpty horizCenter

activate ::
  Offset ->
  NonEmpty (Positioned (Draw path)) ->
  Maybe (Offset, Extents, path)
activate o c =
  getFirst $ foldMap (First . check) c
  where
    check (At o' d) = do
      let e = extentsOf d
      DrawEmbed _ p <- Just d
      guard $ insideBox (o', e) o
      Just (o', e, p)

active :: Eq path => Color -> path -> Collage (Draw path) -> Collage (Draw path)
active color p c = collageCompose offsetZero c (collageSingleton activeZone)
  where
    mkColor (Just path) | path == p = Just color
    mkColor _ = Nothing
    outlineRect =
      rect (lrtb @Natural 1 1 1 1) (DrawCtx $ \mpath _ -> mkColor mpath) (collageExtents c)
    activeZone = DrawEmbed outlineRect p
