{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}

module Source.Path
  ( PathSegment(..),
    Path,
    fromPathSegment,
    Fields,
    Idx(..)
  ) where

import Data.Kind (Type)
import Type.Reflection
import Data.Type.Equality
import Data.Sequence (Seq)

type family Fields (label :: Type) :: [Type]

data Idx xs where
  IZ :: Idx (x : xs)
  IS :: Idx xs -> Idx (x : xs)

deriving instance Show (Idx xs)
deriving instance Eq (Idx xs)

data PathSegment where
  PathSegment :: TypeRep label -> Idx (Fields label) -> PathSegment

instance Eq PathSegment where
  PathSegment t1 a1 == PathSegment t2 a2 =
    case testEquality t1 t2 of
      Nothing -> False
      Just Refl -> a1 == a2

fromPathSegment :: forall label. Typeable label => PathSegment -> Maybe (Idx (Fields label))
fromPathSegment (PathSegment t a) =
  case testEquality t (typeRep @label) of
    Nothing -> Nothing
    Just Refl -> Just a

type Path = Seq PathSegment
