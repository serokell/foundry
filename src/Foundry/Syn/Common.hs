{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Common where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Sequence (Seq)
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Biapplicative
import Data.Void
import Data.Monoid
import Data.Typeable

import Control.Lens
import Control.Monad

import Source.Syntax
import Source.Draw
import Source.Style
import Source.Input
import qualified Source.Input.KeyCode as KeyCode

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

data family Sel :: * -> *

data SomeSel where
  SomeSel
    :: ( Typeable syn
       , Show (Sel syn)
       ) => Sel syn -> SomeSel

instance Show SomeSel where
  showsPrec n (SomeSel sel) = showsPrec n sel

type Path = Seq SomeSel

data Draw' n m
  = Draw' (Draw n m)
  | ActiveZone (Extents n m) Path

active :: (Num n, Ord n, Num m, Ord m) => Path -> Op1 (CollageDraw' n m)
active p c = pure (ActiveZone (getExtents c) p) `mappend` c

activate
  :: forall n m r
   . (Ord n, Num n, Ord m, Num m)
  => (Offset n m -> Extents n m -> Path -> r)
  -> Offset n m
  -> CollageDraw' n m
  -> Maybe r
activate f o = getLast . foldMap (Last . uncurry check) . view _Collage
  where
    within :: (Ord a, Num a) => a -> a -> a -> Bool
    within a zoneOffset zoneExtents
       = a >  zoneOffset
      && a < (zoneOffset + zoneExtents)
    check :: Offset n m -> Draw' n m -> Maybe r
    check o' d = do
      ActiveZone e p <- Just d
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
hover f o c = c <> maybe mempty id (activate obj o c)
  where
    obj o' e _ = offset o' (f (phantom e))

instance DrawPhantom n m (Draw' n m) where
  drawPhantom e = Draw' (drawPhantom e)

instance DrawRectangle n m (Draw' n m) where
  drawRectangle e o b = Draw' (drawRectangle e o b)

instance (Integral n, Integral m) => DrawText n m (Draw' n m) where
  drawText f t = Draw' (drawText f t)

instance HasExtents n m (Draw' n m) where
  getExtents = \case
    Draw' d -> getExtents d
    ActiveZone e _ -> e

type CollageDraw' n m = Collage n m (Draw' n m)

draw' :: (Num n, Num m) => CollageDraw' n m -> CollageDraw n m
draw' c = c >>= \case
  Draw' d -> pure d
  _       -> mempty

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
