{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Source.Point
  ( Point(..)
  , pointX
  , pointY
  , pointZero
  , pointAdd
  , pointMax
  ) where

import Control.Lens
import Control.Applicative

data Point n = Point
  { _pointX :: n
  , _pointY :: n
  } deriving (Eq, Show, Functor)

makeLenses ''Point

instance Applicative Point where
  pure a = Point a a
  Point f1 f2 <*> Point a1 a2 = Point (f1 a1) (f2 a2)

pointZero :: Num n => Point n
pointZero = pure 0

{-# SPECIALIZE pointAdd :: Point Int -> Point Int -> Point Int #-}
pointAdd :: Num n => Point n -> Point n -> Point n
pointAdd = liftA2 (+)

{-# SPECIALIZE pointMax :: Point Int -> Point Int -> Point Int #-}
pointMax :: Ord n => Point n -> Point n -> Point n
pointMax = liftA2 max
