{- |

The purpose of a layouting engine is to take a description of a layout
expressed using primitives (rectangles, circles, lines of text, etc) and
combinators (horizontal/vertical composition, layering, centering, etc) and
compute absolute coordinates for primitives on a 2-dimensional plane.

We represent coordinates and distances in device units (pixels or characters)
using types without a fractional component ('Natural' and 'Integer'). The reason
for this is to guarantee that the resulting collage can be rendered without
undesired anti-aliasing, as our focus is user interfaces and not abstract
vector graphics.

-}

module Source.Layout.Core
  (
    -- * Offset
    Offset(..),
    offsetAdd,
    offsetSub,
    offsetMin,
    offsetMax,
    offsetNegate,
    offsetZero,
    unsafeOffsetExtents,

    -- * Positioned
    Positioned(..),

    -- * Extents
    Extents(..),
    extentsAdd,
    extentsMax,
    extentsOffset,
    extentsWithOffset,
    HasExtents(..),
    heightOf,
    widthOf,

    -- * Margin
    Margin(..),
    marginZero,
    marginMax,

    -- * Baseline
    Baseline(..),
    baselineMin,
    baselineWithOffset,
    HasBaseline(..),

    -- * Collage
    Collage,
    collageSingleton,
    collageAnnotate,
    collageCompose,
    collageComposeN,
    collageExtents,
    collageWidth,
    collageHeight,
    collageMargin,
    collageBaseline,
    collageWithMargin,
    foldMapCollage,
    mapCollageAnnotation,

    -- * LRTB
    LRTB(..),
    lrtb

  ) where

import Data.Void (Void, absurd)
import Numeric.Natural (Natural)
import Data.String (IsString(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (sconcat)

import Source.Layout.Inj

-- | The position of an item (relative or absolute).
data Offset =
  Offset
    { offsetX :: !Integer,
      offsetY :: !Integer
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to offsets,
-- applying it to both dimensions.
offsetOp ::
  (Integer -> Integer -> Integer) ->
  (Offset -> Offset -> Offset)
offsetOp (#) o1 o2 =
  Offset
    { offsetX = offsetX o1 # offsetX o2,
      offsetY = offsetY o1 # offsetY o2 }

-- | Offset pointwise addition.
--
-- >>> offsetAdd (Offset 10 20) (Offset 1 2)
-- Offset {offsetX = 11, offsetY = 22}
--
offsetAdd :: Offset -> Offset -> Offset
offsetAdd = offsetOp (+)

-- | Offset pointwise subtraction.
--
-- >>> offsetSub (Offset 10 20) (Offset 1 2)
-- Offset {offsetX = 9, offsetY = 18}
--
offsetSub :: Offset -> Offset -> Offset
offsetSub = offsetOp (-)

-- | Offset pointwise minimum.
--
-- >>> offsetMin (Offset 10 1) (Offset 2 20)
-- Offset {offsetX = 2, offsetY = 1}
--
offsetMin :: Offset -> Offset -> Offset
offsetMin = offsetOp min

-- | Offset pointwise maximum.
--
-- >>> offsetMax (Offset 10 1) (Offset 2 20)
-- Offset {offsetX = 10, offsetY = 20}
--
offsetMax :: Offset -> Offset -> Offset
offsetMax = offsetOp max

-- | Offset pointwise negation.
--
-- >>> offsetNegate (Offset 5 -10)
-- Offset {offsetX = -5, offsetY = 10}
--
offsetNegate :: Offset -> Offset
offsetNegate (Offset x y) = Offset (negate x) (negate y)

-- | Zero offset.
--
-- prop> offsetAdd offsetZero a = a
-- prop> offsetAdd a offsetZero = a
-- prop> offsetSub a offsetZero = a
-- prop> offsetSub offsetZero a = offsetNegate a
--
-- Note that 'offsetZero' is /not/ an identity element for 'offsetMin' or
-- 'offsetMax' becasue an offset can be negative.
offsetZero :: Offset
offsetZero = Offset 0 0

-- | Convert an offset to extents.
-- Precondition: offset is non-negative, otherwise the function
-- throws @Underflow :: ArithException@.
unsafeOffsetExtents :: Offset -> Extents
unsafeOffsetExtents (Offset x y) = Extents (fromInteger x) (fromInteger y)

-- | Positioned item.
data Positioned a = At { positionedOffset :: !Offset, positionedItem :: !a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The size of an item.
data Extents =
  Extents
    { extentsW :: !Natural,
      extentsH :: !Natural
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to extents,
-- applying it to both dimensions.
extentsOp ::
  (Natural -> Natural -> Natural) ->
  (Extents -> Extents -> Extents)
extentsOp (#) o1 o2 =
  Extents
    { extentsW = extentsW o1 # extentsW o2,
      extentsH = extentsH o1 # extentsH o2 }

-- | Extents pointwise addition.
--
-- >>> extentsAdd (Extents 10 20) (Extents 1 2)
-- Extents {extentsX = 11, extentsY = 22}
--
extentsAdd :: Extents -> Extents -> Extents
extentsAdd = extentsOp (+)

-- | Extents pointwise maximum.
--
-- >>> extentsMax (Extents 10 1) (Extents 2 20)
-- Extents {extentsX = 10, extentsY = 20}
--
extentsMax :: Extents -> Extents -> Extents
extentsMax = extentsOp max

-- | Convert extents to an offset.
extentsOffset :: Extents -> Offset
extentsOffset (Extents w h) = Offset (toInteger w) (toInteger h)

-- | Compute the extents for a subcollage in a composition.
--
-- Precondition: offset is non-negative, otherwise the function
-- throws @Underflow :: ArithException@.
extentsWithOffset :: Offset -> Extents -> Extents
extentsWithOffset offset = extentsAdd (unsafeOffsetExtents offset)

-- | A class of items that have extents.
class HasExtents a where
  extentsOf :: a -> Extents

heightOf, widthOf :: HasExtents a => a -> Natural
heightOf = extentsH . extentsOf
widthOf = extentsW . extentsOf

-- | A minimum recommended distance from an item to any other item.
data Margin =
  Margin
    { marginLeft :: !Natural,
      marginRight :: !Natural,
      marginTop :: !Natural,
      marginBottom :: !Natural
    } deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to margins,
-- applying it to all four directions.
marginOp ::
  (Natural -> Natural -> Natural) ->
  (Margin -> Margin -> Margin)
marginOp (#) m1 m2 =
  Margin
    { marginLeft = marginLeft m1 # marginLeft m2,
      marginRight = marginRight m1 # marginRight m2,
      marginTop = marginTop m1 # marginTop m2,
      marginBottom = marginBottom m1 # marginBottom m2 }

-- | Zero margin.
marginZero :: Margin
marginZero = Margin 0 0 0 0

-- | Margin pointwise maximum.
--
-- >>> marginMax (Margin 1 10 2 20) (Margin 3 4 5 6)
-- Margin {marginLeft = 3, marginRight = 10, marginTop = 5, marginBottom = 20}
--
marginMax :: Margin -> Margin -> Margin
marginMax = marginOp max

-- | The imaginary line upon which the topmost line of text rests, expressed as
-- a distance from the top edge. May not be present if the item does not
-- contain text.
data Baseline = NoBaseline | Baseline Natural
  deriving (Eq, Ord, Show)

-- | Lift a binary numeric operation to baselines.
baselineOp ::
  (Natural -> Natural -> Natural) ->
  (Baseline -> Baseline -> Baseline)
baselineOp _ NoBaseline l2 = l2
baselineOp _ l1 NoBaseline = l1
baselineOp (#) (Baseline l1) (Baseline l2) = Baseline (l1 # l2)

-- | Baseline minimum.
--
-- >>> baselineMin (Baseline 10) (Baseline 20)
-- Baseline 10
--
-- >>> baselineMin (Baseline 20) NoBaseline
-- Baseline 20
--
baselineMin :: Baseline -> Baseline -> Baseline
baselineMin = baselineOp min

-- | Compute the baseline for a subcollage in a composition.
--
-- Precondition: offset is non-negative, otherwise the function
-- throws @Underflow :: ArithException@.
baselineWithOffset :: Offset -> Baseline -> Baseline
baselineWithOffset _ NoBaseline = NoBaseline
baselineWithOffset offset (Baseline l1) =
  Baseline (l1 + fromInteger (offsetY offset))

-- | A class of items that have a baseline.
class HasBaseline a where
  baselineOf :: a -> Baseline

-- | A collage of elements. Can be created from a single element with
-- 'collageSingleton' or from a combination of several subcollages with
-- relative offsets from a point with 'collageComposeN'. After a collage is
-- built, it can be folded with 'foldMapCollage'.
--
-- Here's a visualisation of a collage with two rectangular elements:
--
-- @
--      top-left corner
--     /
--    *   +---+
--        |   |
--    +-+ +---+
--    | |
--    +-+     *
--             \\
--             bottom-right corner
-- @
--
-- The bounding box (extents) of a collage is a vector from its top-left corner
-- to the bottom-right corner.
--
data Collage n a =
  Collage !Margin !Extents !Baseline !(Offset -> CollageBuilder n a)

instance HasExtents (Collage n a) where
  extentsOf = collageExtents

instance HasBaseline (Collage n a) where
  baselineOf = collageBaseline

-- | Get the bounding box of a collage in constant time.
collageExtents :: Collage n a -> Extents
collageExtents (Collage _ e _ _) = e

-- | Get the margin of a collage in constant time.
collageMargin :: Collage n a -> Margin
collageMargin (Collage m _ _ _) = m

-- | Get the baseline in constant time.
collageBaseline :: Collage n a -> Baseline
collageBaseline (Collage _ _ l _) = l

collageBuilder :: Collage n a -> Offset -> CollageBuilder n a
collageBuilder (Collage _ _ _ b) = b

-- | Get the width of a collage in constant time.
collageWidth :: Collage n a -> Natural
collageWidth = extentsW . collageExtents

-- | Get the height of a collage in constant time.
collageHeight :: Collage n a -> Natural
collageHeight = extentsH . collageExtents

-- | Set the margins of a collage to the pointwise maximum of their current
-- value and the specified new value. Taking the maximum ensures we do not
-- erase the margins computed from subcollages.
collageWithMargin :: Margin -> Collage n a -> Collage n a
collageWithMargin m' (Collage m e l b) =
  Collage (marginMax m' m) e l b

-- | Fold over the collage elements with absolute positions,
-- ordered by z-index (ascending).
--
-- O(n) - linear in the amount of elements.
--
-- The input offset is the position for the top-left corner of the collage.
foldMapCollage ::
  Semigroup s =>
  (Positioned a -> s) ->
  Offset ->
  Collage n a ->
  (n, s)
foldMapCollage yield offset (Collage _ _ _ b) =
  case b offset of
    CollageBuilder n b' -> (n, b' yield)

data CollageBuilder n a =
  CollageBuilder !n !(forall r. Semigroup r => (Positioned a -> r) -> r)

collageBuilderSingleton :: n -> a -> Offset -> CollageBuilder n a
collageBuilderSingleton n a =
  \offset -> CollageBuilder n ($ At offset a)

instance Semigroup n => Semigroup (CollageBuilder n a) where
  CollageBuilder n1 b1 <> CollageBuilder n2 b2 =
    CollageBuilder (n1 <> n2) (b1 <> b2)

-- | Construct a collage from a single element.
collageSingleton :: (HasExtents a, HasBaseline a, Monoid n) => a -> Collage n a
collageSingleton a =
  Collage marginZero (extentsOf a) (baselineOf a) (collageBuilderSingleton mempty a)

instance (HasExtents a, HasBaseline a, IsString a, Monoid n) => IsString (Collage n a) where
  fromString = collageSingleton . fromString

-- | Add an annotation to a collage.
collageAnnotate :: Semigroup n => (Offset -> n) -> Collage n a -> Collage n a
collageAnnotate mkann (Collage m e l b) =
    Collage m e l (liftA2 withAnn mkann b)
  where
    withAnn ann = mapCollageBuilderAnnotation (<> ann)

-- | Modify the collage annotation.
mapCollageAnnotation :: (n -> n') -> Collage n a -> Collage n' a
mapCollageAnnotation f (Collage m e l b) = Collage m e l (mapCollageBuilderAnnotation f . b)

mapCollageBuilderAnnotation :: (n -> n') -> CollageBuilder n a -> CollageBuilder n' a
mapCollageBuilderAnnotation f (CollageBuilder n b) = CollageBuilder (f n) b

-- | Combine a pair of collages by placing one atop another
-- with an offset. For instance, an @'Offset' a b@ would yield
-- the following result:
--
-- @
-- +------------+ collage below (first argument)
-- |     ^      |
-- |     |b     |
-- |  a  v      |
-- | \<-\> +------------+ collage above (second argument)
-- |     |            |
-- +-----|            |
--       |            |
--       +------------+
-- @
--
-- This is a special case of 'collageComposeN'.
--
collageCompose ::
  Semigroup n =>
  Offset ->
  Collage n a ->
  Collage n a ->
  Collage n a
collageCompose offset c1 c2 =
  positionedItem (At offsetZero c1 <> At offset c2)

data MarginPoints =
  MarginPoints
    !Offset
    !Offset

instance Semigroup MarginPoints where
  MarginPoints p1 q1 <> MarginPoints p2 q2 =
    MarginPoints (offsetMin p1 p2) (offsetMax q1 q2)

toMarginPoints :: Offset -> Extents -> Margin -> MarginPoints
toMarginPoints offset extents margin = MarginPoints p q
  where
    p = offsetAdd offset marginTopLeftOffset
    q = offsetAdd offset marginBottomRightOffset
    marginTopLeftOffset =
      Offset
        { offsetX = (negate . toInteger) (marginLeft margin),
          offsetY = (negate . toInteger) (marginTop margin) }
    marginBottomRightOffset =
      Offset
        { offsetX = toInteger (marginRight margin) + toInteger (extentsW extents),
          offsetY = toInteger (marginBottom margin) + toInteger (extentsH extents) }

fromMarginPoints :: Extents -> MarginPoints -> Margin
fromMarginPoints extents marginPoints =
  Margin
    { marginLeft = (intNatCeil . negate) (offsetX p),
      marginRight = intNatCeil (offsetX q - offsetX eOffset),
      marginTop = (intNatCeil . negate) (offsetY p),
      marginBottom = intNatCeil (offsetY q - offsetY eOffset) }
  where
    eOffset = extentsOffset extents
    MarginPoints p q = marginPoints

    intNatCeil :: Integer -> Natural
    intNatCeil = fromInteger . max 0

-- Lazy fields to tie the knot in collageComposeN (minOffset)
data CollageComposeAccum n a =
  CollageComposeAccum
    Offset
    MarginPoints
    Extents
    Baseline
    (Offset -> CollageBuilder n a)

instance Semigroup n => Semigroup (CollageComposeAccum n a) where
  CollageComposeAccum o1 mp1 e1 l1 b1 <> CollageComposeAccum o2 mp2 e2 l2 b2 =
    CollageComposeAccum (offsetMin o1 o2) (mp1 <> mp2) (extentsMax e1 e2) (baselineMin l1 l2) (b1 <> b2)

-- | A generalization of 'collageCompose' to take a non-empty list of
-- subcollages instead of a pair.
--
-- Offset common between all elements is factored out into the position of the
-- resulting collage.
--
collageComposeN ::
  forall n a.
  Semigroup n =>
  NonEmpty (Positioned (Collage n a)) ->
  Positioned (Collage n a)
collageComposeN (positionedCollage :| []) =
  -- This special case is an optimization and does not affect the semantics.
  positionedCollage
collageComposeN elements =
  At minOffset resultCollage
  where
    resultCollage =
      Collage resultMargin resultExtents resultBaseline resultElements

    resultMargin = fromMarginPoints resultExtents resultMarginPoints

    (CollageComposeAccum minOffset resultMarginPoints resultExtents resultBaseline resultElements) =
      sconcat (fmap @NonEmpty processElement elements)

    processElement ::
      Positioned (Collage n a) ->
      CollageComposeAccum n a
    processElement (At offset collage) =
      let
        extents = collageExtents collage
        margin = collageMargin collage
        baseline = collageBaseline collage
        -- normalized offset, guaranteed to be non-negative
        offset' = offsetSub offset minOffset
        extents' = extentsWithOffset offset' extents
        baseline' = baselineWithOffset offset' baseline
        marginPoints = toMarginPoints offset' extents margin
        element' = collageBuilder collage . offsetAdd offset'
      in
        CollageComposeAccum offset marginPoints extents' baseline' element'

instance Semigroup n => Semigroup (Positioned (Collage n a)) where
  a <> b = sconcat (a :| b : [])
  sconcat = collageComposeN

instance (HasExtents a, HasBaseline a, Inj p a, Monoid n) => Inj p (Collage n a) where
  inj = collageSingleton . inj

-- | A value for each side: left, right, top, bottom.
data LRTB a = LRTB
  { left :: !a,
    right :: !a,
    top :: !a,
    bottom :: !a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative LRTB where
  pure a = LRTB a a a a
  LRTB lf rf tf bf <*> LRTB la ra ta ba =
    LRTB (lf la) (rf ra) (tf ta) (bf ba)

instance (Inj p' a, p ~ LRTB p') => Inj p (LRTB a) where
  inj = fmap inj

-- | Construct and inject an 'LRTB' value.
lrtb :: forall p a. Inj (LRTB p) a => p -> p -> p -> p -> a
lrtb l r t b = inj (LRTB l r t b)

instance Num a => Num (LRTB a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance {-# OVERLAPPING #-} Inj Void (LRTB a) where
  inj = absurd

