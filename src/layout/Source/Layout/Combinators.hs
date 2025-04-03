module Source.Layout.Combinators
  ( substrate,
    collageMarginBB,
    collageAnnotateMargin,
    horiz,
    horizTop,
    horizBottom,
    horizCenter,
    horizBaseline,
    vert,
    vertLeft,
    vertRight,
    vertCenter,
    insideBox,
    integralDistribExcess
  ) where

import Numeric.Natural
import Data.List.NonEmpty
import Source.Layout.Core

substrate ::
  Semigroup n =>
  LRTB Natural ->
  (Extents -> Collage n a) ->
  Collage n a ->
  Collage n a
substrate pad mkSub collage =
  collageCompose
    Offset
      { offsetX = toInteger $ left pad,
        offsetY = toInteger $ top pad }
    (mkSub extents)
    collage
  where
    e = collageExtents collage
    extents = Extents
      { extentsW = left pad + extentsW e + right pad,
        extentsH = top pad + extentsH e + bottom pad }

-- | Collage margin bounding box. The offset is negative (top-left margin).
collageMarginBB :: Collage n a -> (Offset, Extents)
collageMarginBB collage = (offset, extents)
  where
    e = collageExtents collage
    m = collageMargin collage
    extents = Extents
      { extentsW = marginLeft m + extentsW e + marginRight m,
        extentsH = marginTop m + extentsH e + marginBottom m }
    offset = Offset
      { offsetX = negate . toInteger $ marginLeft m,
        offsetY = negate . toInteger $ marginTop m }

collageAnnotateMargin ::
  Semigroup n =>
  ((Offset, Extents) -> n) ->
  Collage n a ->
  Collage n a
collageAnnotateMargin ann collage =
    collageAnnotate mkAnn collage
  where
    mkAnn offset' = ann (offsetAdd offset offset', extents)
    (offset, extents) = collageMarginBB collage

horiz, vert ::
  Semigroup n =>
  (Collage n a -> Integer) ->
  Collage n a ->
  Collage n a ->
  Collage n a
horiz align c1 c2 =
  positionedItem $
    collageComposeN (At offset1 c1 :| At offset2 c2 : [])
  where
    m1 = collageMargin c1
    m2 = collageMargin c2
    marginX = max (marginRight m1) (marginLeft m2)
    offsetX = toInteger (widthOf c1 + marginX)
    offset1 = Offset{offsetY=align c1, offsetX=0}
    offset2 = Offset{offsetY=align c2, offsetX}
vert align c1 c2 =
  positionedItem $
    collageComposeN (At offset1 c1 :| At offset2 c2 : [])
  where
    m1 = collageMargin c1
    m2 = collageMargin c2
    marginY = max (marginBottom m1) (marginTop m2)
    offsetY = toInteger (heightOf c1 + marginY)
    offset1 = Offset{offsetX=align c1, offsetY=0}
    offset2 = Offset{offsetX=align c2, offsetY}

horizTop, horizBottom, horizCenter, horizBaseline ::
  Semigroup n => Collage n a -> Collage n a -> Collage n a
horizTop = horiz (const 0)
horizBottom = horiz (negate . toInteger . heightOf)
horizCenter = horiz (negate . toInteger . (`quot` 2) . heightOf)
horizBaseline = horiz (negate . toInteger . collageBaselineDefault)

collageBaselineDefault :: Collage n a -> Natural
collageBaselineDefault c =
  case collageBaseline c of
    NoBaseline -> heightOf c
    Baseline a -> a

vertLeft, vertRight, vertCenter ::
  Semigroup n => Collage n a -> Collage n a -> Collage n a
vertLeft = vert (const 0)
vertRight = vert (negate . toInteger . widthOf)
vertCenter = vert (negate . toInteger . (`quot` 2) . widthOf)

insideBox :: (Offset, Extents) -> Offset -> Bool
insideBox (Offset ax ay, Extents w h) (Offset x y) =
  inRange (ax, bx) x &&
  inRange (ay, by) y
  where
    bx = ax + toInteger w
    by = ay + toInteger h
    inRange (lower, upper) =
      liftA2 (&&) (lower<=) (upper>=)

integralDistribExcess :: Integral n => n -> n -> (n, n)
integralDistribExcess desired actual = (l, r)
  where
    excess =
      if desired > actual
      then desired - actual
      else 0
    l = excess `quot` 2
    r = excess - l
