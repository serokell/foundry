{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn.Common where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Monoid
import Data.Maybe

import Control.Lens
import Control.Monad

import qualified Source.Collage.Builder as CB
import Source.Draw
import Source.Style
import Source.Input
import Source.Syntax

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

type Path = Seq Int

newtype ActiveZone = ActiveZone Path

type CollageDraw' n = CB.CollageBuilder n (Draw ActiveZone)

active :: (Num n, Ord n) => Path -> Op1 (CollageDraw' n)
active p c = (CB.collageBuilder . collage1 (CB.getExtents c)) activeZone `mappend` c
  where
    activeZone = DrawEmbed (ActiveZone p)

within :: (Ord n, Num n) => n -> n -> n -> Bool
within a zoneOffset zoneExtents
   = a >  zoneOffset
  && a < (zoneOffset + zoneExtents)

activate
  :: (Ord n, Num n)
  => Offset n
  -> Collage n (Draw ActiveZone)
  -> Maybe (Element n Path)
activate o = getLast . foldMap (Last . check) . getCollage
  where
    check (Element o' e d) = do
      DrawEmbed (ActiveZone p) <- Just d
      let Point okX okY = Point within within <*> o <*> o' <*> e
      guard (okX && okY)
      Just (Element o' e p)

hover
  :: (Ord n, Num n)
  => Op1 (CollageDraw' n)
  -> Offset n
  -> Op1 (CollageDraw' n)
hover f o c = CB.collageBuilder c' <> maybe mempty obj (activate o c')
  where
    c' = CB.buildCollage c
    obj el = CB.offset
      (el ^. elementOffset)
      (f (phantom (el ^. elementExtents)))

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

guardInputEvent :: (InputEvent n -> Bool) -> React n la syn
guardInputEvent = guard <=< views rctxInputEvent

class UndoEq a where
  undoEq :: a -> a -> Bool

class SynSelection a sel | a -> sel where
  synSelection :: a -> Maybe sel
  synSelection = const Nothing

synSelectionSelf :: SynSelection a sel => a -> Bool
synSelectionSelf = isNothing . synSelection
