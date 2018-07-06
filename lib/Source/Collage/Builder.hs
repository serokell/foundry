{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module Source.Collage.Builder
  ( CollageBuilder
  , collageBuilder
  , collageBuilder1
  , buildCollage
  , getExtents
  , offset
  , overlay
  , overlays
  , vertical
  , horizontal
  ) where

import Control.Lens

import qualified Source.Collage as C
import           Source.Collage
  ( Collage
  , Extents
  , Offset
  , Op1
  , Op2
  , OpN
  )

data FreeCollage n a
  = PureFC (Collage n a)
  | OffsetFC (Offset n) (FreeCollage n a)
  | MappendFC (FreeCollage n a) (FreeCollage n a)
  deriving (Eq, Show)

rigidCollage :: Num n => FreeCollage n a -> Collage n a
rigidCollage = rigidCollage' C.pointZero

rigidCollage' :: Num n => Offset n -> FreeCollage n a -> Collage n a
rigidCollage' o = \case
  PureFC         c  -> C.offset o c
  OffsetFC   o' fc  -> rigidCollage' (C.pointAdd o o') fc
  MappendFC fc1 fc2 -> rigidCollage' o fc1 <> rigidCollage' o fc2

data CollageBuilder n a = CollageBuilder
  { _collageBuilderCollage :: FreeCollage n a
  , _collageBuilderExtents :: Extents n
  } deriving (Eq, Show)

makeLenses ''CollageBuilder

collageBuilder
  :: (Num n, Ord n)
  => Collage n a
  -> CollageBuilder n a
collageBuilder c = CollageBuilder (PureFC c) (C.collageGetExtents c)

collageBuilder1 :: (Num n, Ord n) => Extents n -> a -> CollageBuilder n a
collageBuilder1 e = collageBuilder . C.collage1 e

getExtents :: CollageBuilder n a -> Extents n
getExtents = view collageBuilderExtents

buildCollage :: Num n => CollageBuilder n a -> Collage n a
buildCollage = rigidCollage . view collageBuilderCollage

instance (Num n, Ord n) => Semigroup (CollageBuilder n a) where
  cb1 <> cb2 = CollageBuilder
    (view collageBuilderCollage cb1 `MappendFC`  view collageBuilderCollage cb2)
    (view collageBuilderExtents cb1 `C.pointMax` view collageBuilderExtents cb2)

instance (Num n, Ord n) => Monoid (CollageBuilder n a) where
  mempty = CollageBuilder (PureFC mempty) C.pointZero

offset :: Num n => Offset n -> Op1 (CollageBuilder n a)
offset o
  = over collageBuilderCollage (OffsetFC o)
  . over collageBuilderExtents (C.pointAdd o)

overlay
  :: (Num n, Ord n)
  => (Extents n -> Offset n)
  -> Op2 (CollageBuilder n a)
overlay move cb1 cb2 =
  let o = move (view collageBuilderExtents cb1)
  in cb1 <> offset o cb2

overlays
  :: (Num n, Ord n)
  => (Extents n -> Offset n)
  -> OpN (CollageBuilder n a)
overlays move = foldr (overlay move) mempty

vertical :: (Num n, Ord n) => OpN (CollageBuilder n a)
vertical = overlays (set C.pointX 0)

horizontal :: (Num n, Ord n) => OpN (CollageBuilder n a)
horizontal = overlays (set C.pointY 0)
