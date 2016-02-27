{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Common where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Biapplicative
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

text :: Text -> CollageDraw' Int Int
text = textline font

punct :: Text -> CollageDraw' Int Int
punct = textline (font { fontColor = light1 })

font :: Font
font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal

keyLetter :: Char -> KeyCode -> Bool
keyLetter c keyCode = keyChar keyCode == Just c

keyCodeLetter :: KeyCode -> Char -> InputEvent n m -> Bool
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

type CollageDraw' n m = CB.CollageBuilder n m (Draw n m ActiveZone)

active :: (Num n, Ord n, Num m, Ord m) => Path -> Op1 (CollageDraw' n m)
active p c = (CB.collageBuilder . pure) activeZone `mappend` c
  where
    activeZone = DrawEmbed (getExtents c) (ActiveZone p)

activate
  :: forall n m r
   . (Ord n, Num n, Ord m, Num m)
  => (Offset n m -> Extents n m -> Path -> r)
  -> Offset n m
  -> Collage n m (Draw n m ActiveZone)
  -> Maybe r
activate f o = getLast . foldMap (Last . uncurry check) . getCollage
  where
    within :: (Ord a, Num a) => a -> a -> a -> Bool
    within a zoneOffset zoneExtents
       = a >  zoneOffset
      && a < (zoneOffset + zoneExtents)
    check :: Offset n m -> Draw n m ActiveZone -> Maybe r
    check o' d = do
      DrawEmbed e (ActiveZone p) <- Just d
      guard
        $ uncurry (&&)
        $ biliftA3 within within o o' e
      Just (f o' e p)

hover
  :: forall n m
   . (Ord n, Num n, Ord m, Num m)
  => Op1 (CollageDraw' n m)
  -> Offset n m
  -> Op1 (CollageDraw' n m)
hover f o c = c <> maybe mempty id (activate obj o (CB.buildCollage c))
  where
    obj o' e _ = CB.offset o' (f (phantom e))

data LayoutCtx = LayoutCtx
  { _lctxSelected :: Bool
  , _lctxPath     :: Path
  }

makeLenses ''LayoutCtx

sel :: LayoutCtx -> CollageDraw' Int Int -> CollageDraw' Int Int
sel lctx
  = active (lctx ^. lctxPath)
  . if lctx ^. lctxSelected
    then outline dark2 . background dark3
    else id

class UndoEq a where
  undoEq :: a -> a -> Bool
