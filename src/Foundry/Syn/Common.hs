{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Common where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Monoid
import Data.Typeable

import Control.Lens
import Control.Monad

import qualified Source.Collage.Builder as CB
import Source.Draw
import Source.Style
import Source.Input

dark1, dark2, dark3, light1 :: Color
dark1  = RGB 0.20 0.20 0.20
dark2  = RGB 0.30 0.30 0.30
dark3  = RGB 0.25 0.25 0.25
light1 = RGB 0.70 0.70 0.70

text :: Text -> CollageDraw' Int
text = textline font

punct :: Text -> CollageDraw' Int
punct = textline (font { fontColor = light1 })

font :: Font
font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal

keyLetter :: Char -> KeyCode -> Bool
keyLetter c keyCode = keyChar keyCode == Just c

keyCodeLetter :: KeyCode -> Char -> InputEvent n -> Bool
keyCodeLetter kc c = \case
  KeyPress [] keyCode -> keyCode == kc || keyLetter c keyCode
  _ -> False

data family SEL :: label -> *
data family SYN :: label -> *

data SomeSel where
  SomeSel
    :: ( Typeable (SEL label)
       , Show (SEL label)
       ) => SEL label -> SomeSel

instance Show SomeSel where
  showsPrec n (SomeSel sel) = showsPrec n sel

type Path = Seq SomeSel

newtype ActiveZone = ActiveZone Path

type CollageDraw' n = CB.CollageBuilder n (Draw ActiveZone)

active :: (Num n, Ord n) => Path -> Op1 (CollageDraw' n)
active p c = (CB.collageBuilder . collage1 (CB.getExtents c)) activeZone `mappend` c
  where
    activeZone = DrawEmbed (ActiveZone p)

activate
  :: forall n r
   . (Ord n, Num n)
  => (Offset n -> Extents n -> Path -> r)
  -> Offset n
  -> Collage n (Draw ActiveZone)
  -> Maybe r
activate f o = getLast . foldMap (Last . check) . getCollage
  where
    within :: (Ord a, Num a) => a -> a -> a -> Bool
    within a zoneOffset zoneExtents
       = a >  zoneOffset
      && a < (zoneOffset + zoneExtents)
    check :: Element n (Draw ActiveZone) -> Maybe r
    check (Element o' e d) = do
      DrawEmbed (ActiveZone p) <- Just d
      let Point okX okY = Point within within <*> o <*> o' <*> e
      guard (okX && okY)
      Just (f o' e p)

hover
  :: (Ord n, Num n)
  => Op1 (CollageDraw' n)
  -> Offset n
  -> Op1 (CollageDraw' n)
hover f o c = CB.collageBuilder c' <> maybe mempty id (activate obj o c')
  where
    c' = CB.buildCollage c
    obj o' e _ = CB.offset o' (f (phantom e))

data LayoutCtx = LayoutCtx
  { _lctxSelected :: Bool
  , _lctxPath     :: Path
  }

makeLenses ''LayoutCtx

sel :: LayoutCtx -> CollageDraw' Int -> CollageDraw' Int
sel lctx
  = active (lctx ^. lctxPath)
  . if lctx ^. lctxSelected
    then outline dark2 . background dark3
    else id

class UndoEq a where
  undoEq :: a -> a -> Bool
