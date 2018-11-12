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

import Source.Path

data CursorBlink = CursorVisible | CursorInvisible

blink :: CursorBlink -> CursorBlink
blink = \case
  CursorVisible -> CursorInvisible
  CursorInvisible -> CursorVisible

data Paths =
  Paths
    { pathsCursor :: Maybe Path,
      pathsSelection :: Path }

data DrawCtx a = DrawCtx (Paths -> CursorBlink -> a)
  deriving (Functor)

instance Inj p a => Inj p (DrawCtx a) where
  inj p = DrawCtx (\_ _ -> inj p)

withDrawCtx :: Paths -> CursorBlink -> DrawCtx a -> a
withDrawCtx paths curBlink (DrawCtx f) = f paths curBlink

data Draw
  = DrawCairoElement (CairoElement DrawCtx)
  | DrawEmbed (CairoElement DrawCtx) Path

instance HasExtents Draw where
  extentsOf = extentsOf . toCairoElementDraw

toCairoElementDraw :: Draw -> CairoElement DrawCtx
toCairoElementDraw = \case
  DrawCairoElement ce -> ce
  DrawEmbed ce _ -> ce

instance g ~ DrawCtx => Inj (CairoElement g) Draw where
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

textline :: Color -> Font -> Text -> (Paths -> CursorBlink -> Maybe Natural) -> Collage Draw
textline color font str cur = text font (inj color) str (DrawCtx cur)

line :: Color -> Natural -> Collage Draw
line color w = rect nothing (inj color) (Extents w 1)

centerOf :: Extents -> Collage Draw -> LRTB Natural
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

horizontal, vertical, horizontalCenter :: NonEmpty (Collage Draw) -> Collage Draw
horizontal = foldr1 @NonEmpty horizTop
vertical = foldr1 @NonEmpty vertLeft
horizontalCenter = foldr1 @NonEmpty horizCenter

findPath ::
  Offset ->
  NonEmpty (Positioned Draw) ->
  Maybe Path
findPath o c =
  getFirst $ foldMap (First . check) c
  where
    check (At o' d) = do
      let e = extentsOf d
      DrawEmbed _ p <- Just d
      guard $ insideBox (o', e) o
      Just p
