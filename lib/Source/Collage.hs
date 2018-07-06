{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
module Source.Collage
  ( Op1
  , Op2
  , OpN
  , module Source.Point
  , Offset
  , Extents
  , Element(..)
  , elementOffset
  , elementExtents
  , elementObject
  , Collage(..)
  , _Collage
  , collage1
  , collageGetExtents
  , offset
  , getExcess
  ) where

import Control.Lens

import Source.Point

type Op1 a = a -> a
type Op2 a = a -> a -> a
type OpN a = [a] -> a

type Offset = Point
type Extents = Point

data Element n a = Element
  { _elementOffset  :: Offset n
  , _elementExtents :: Extents n
  , _elementObject  :: a
  } deriving (Eq, Show, Functor)

makeLenses ''Element

newtype Collage n a = Collage { getCollage :: [Element n a] }
  deriving (Eq, Show, Semigroup, Monoid, Functor)

makePrisms ''Collage

collage1 :: Num n => Extents n -> a -> Collage n a
collage1 e a = Collage [Element pointZero e a]

{-# SPECIALIZE offset :: Offset Int -> Op1 (Collage Int a) #-}
offset :: Num n => Offset n -> Op1 (Collage n a)
offset o = over (_Collage . mapped . elementOffset) (pointAdd o)

collageGetExtents :: (Num n, Ord n) => Collage n a -> Extents n
collageGetExtents
  = foldr pointMax pointZero
  . map getElementBottomRight
  . getCollage
  where
    getElementBottomRight :: Num n => Element n a -> Extents n
    getElementBottomRight a =
      pointAdd
        (a ^. elementOffset)
        (a ^. elementExtents)

getExcess :: Integral n => n -> n -> (n, n)
getExcess vacant actual =
  let
    excess = max 0 (vacant - actual)
    excess1 = excess `quot` 2
    excess2 = excess - excess1
  in (excess1, excess2)
