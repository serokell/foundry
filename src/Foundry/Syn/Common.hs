module Foundry.Syn.Common where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Monoid
import Data.Vinyl
import Data.Dynamic

import Control.Lens
import Control.Applicative
import Control.Monad

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

text :: s -/ Draw ActiveZone => Text -> Collage s
text = textline font

punct :: s -/ Draw ActiveZone => Text -> Collage s
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

active :: s -/ Draw ActiveZone => Path -> Collage s -> Collage s
active p c = inj activeZone <> c
  where
    activeZone = DrawEmbed (collageExtents c) (ActiveZone p)

activate ::
  (s -/ Draw ActiveZone, Element s ~ Draw ActiveZone) =>
  Offset ->
  Collage s ->
  Maybe (Offset, Extents, Path)
activate o c =
  getLast . foldMap (Last . check) $ collageRepElements (getCollageRep c)
  where
    check (o', e, d) = do
      DrawEmbed _ (ActiveZone p) <- Just d
      guard $ insideBox (o', e) o
      Just (o', e, p)

-- TODO: remove (Element s ~ Draw ActiveZone) by a using a different hover
--       system (adding hover intersection into prim context perhaps)
hover ::
  (s -/ Draw ActiveZone, Element s ~ Draw ActiveZone) =>
  (Collage s -> Collage s) ->
  Offset ->
  Collage s ->
  Collage s
hover f o c =
  case activate o c of
    Nothing -> c
    Just (o', e, _) -> collageCompose o' c (f (phantom e))

data LayoutCtx = LayoutCtx
  { _lctxSelected :: Bool
  , _lctxPath     :: Path
  }

makeLenses ''LayoutCtx

sel :: s -/ Draw ActiveZone => LayoutCtx -> Collage s -> Collage s
sel lctx
  = active (lctx ^. lctxPath)
  . if lctx ^. lctxSelected
    then outline dark2 . background dark3
    else id

simpleSubreact :: Char -> syn -> Subreact rp la syn
simpleSubreact c syn = do
  KeyPress [Shift] keyCode <- view rctxInputEvent
  guard (keyLetter c keyCode)
  return syn

guardInputEvent :: (InputEvent Int -> Bool) -> React rp la syn
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
