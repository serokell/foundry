module Foundry.Syn.Common where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Monoid
import Data.Vinyl
import Data.Dynamic

import Control.Lens
import Control.Applicative
import Control.Monad

import qualified Source.Collage.Builder as CB
import Source.Draw
import Source.Style
import Source.Input
import Source.Syntax

maybeA :: Alternative f => Maybe a -> f a
maybeA = maybe empty pure

dark1, dark2, dark3, light1 :: Color
dark1  = RGB 0.20 0.20 0.20
dark2  = RGB 0.30 0.30 0.30
dark3  = RGB 0.25 0.25 0.25
light1 = RGB 0.70 0.70 0.70

text :: Integral n => Text -> CollageDraw' n
text = textline font

punct :: Integral n => Text -> CollageDraw' n
punct = textline (font { fontColor = light1 })

font :: Font
font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal

keyLetter :: Char -> KeyCode -> Bool
keyLetter c keyCode = keyChar keyCode == Just c

keyCodeLetter :: KeyCode -> Char -> InputEvent n -> Bool
keyCodeLetter kc c = \case
  KeyPress [] keyCode -> keyCode == kc || keyLetter c keyCode
  _ -> False

type Path = Seq Dynamic

newtype ActiveZone = ActiveZone Path

type CollageDraw' n = CB.CollageBuilder n (Draw ActiveZone)

active :: (Num n, Ord n) => Path -> Op1 (CollageDraw' n)
active p c = CB.collageBuilder1 (CB.getExtents c) activeZone <> c
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

sel :: (Num n, Ord n) => LayoutCtx -> CollageDraw' n -> CollageDraw' n
sel lctx
  = active (lctx ^. lctxPath)
  . if lctx ^. lctxSelected
    then outline dark2 . background dark3
    else id

simpleSubreact :: Char -> syn -> Subreact n rp la syn
simpleSubreact c syn = do
  KeyPress [Shift] keyCode <- view rctxInputEvent
  guard (keyLetter c keyCode)
  return syn

guardInputEvent :: (InputEvent n -> Bool) -> React n rp la syn
guardInputEvent = guard <=< views rctxInputEvent

class UndoEq a where
  undoEq :: a -> a -> Bool

instance UndoEq (Rec f '[]) where
  undoEq RNil RNil = True

instance (UndoEq (f a), UndoEq (Rec f as))
      => UndoEq (Rec f (a ': as)) where
  undoEq (a1 :& as1) (a2 :& as2) = undoEq a1 a2 && undoEq as1 as2


class SynSelfSelected a where
  synSelfSelected :: a -> Bool
  default synSelfSelected :: SynSelection a sel => a -> Bool
  synSelfSelected = view synSelectionSelf

class SynSelfSelected a => SynSelection a sel | a -> sel where
  synSelection :: Lens' a sel
  synSelectionSelf :: Lens' a Bool
