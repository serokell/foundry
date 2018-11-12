module Foundry.Syn.Common where

import Data.Text (Text)
import Numeric.Natural (Natural)

import Control.Lens
import Control.Applicative
import Control.Monad

import Source.Draw
import Source.Input
import Source.Syntax

maybeA :: Alternative f => Maybe a -> f a
maybeA = maybe empty pure

dark1, dark2, dark3, light1, white :: Color
dark1  = RGB 51 51 51
dark2  = RGB 77 77 77
dark3  = RGB 64 64 64
light1 = RGB 179 179 179
white  = RGB 255 255 255

textWithCursor :: Text -> (Paths -> CursorBlink -> Maybe Natural) -> Collage Draw
textWithCursor = textline white font

text :: Text -> Collage Draw
text t = textWithCursor t (\_ _ -> Nothing)

punct :: Text -> Collage Draw
punct t = textline light1 font t (\_ _ -> Nothing)

font :: Font
font = Font "Ubuntu" 12 FontWeightNormal

keyLetter :: Char -> KeyCode -> Bool
keyLetter c keyCode = keyChar keyCode == Just c

keyCodeLetter :: KeyCode -> Char -> InputEvent n -> Bool
keyCodeLetter kc c = \case
  KeyPress [] keyCode -> keyCode == kc || keyLetter c keyCode
  _ -> False

layoutSel :: Path -> Collage Draw -> Collage Draw
layoutSel path =
    active path
  . substrate (lrtb @Natural 0 0 0 0) (\e ->
      let
        mkColor color = DrawCtx $ \Paths{..} _ ->
          if pathsSelection == path then inj color else nothing
        background = rect nothing (mkColor dark3) e
        border = rect (lrtb @Natural 1 1 1 1) (mkColor dark2) e
      in
        collageCompose offsetZero background border)

active :: Path -> Collage Draw -> Collage Draw
active p c = collageCompose offsetZero c (collageSingleton activeZone)
  where
    mkColor (Just path) | path == p = Just light1
    mkColor _ = Nothing
    outlineRect =
      rect (lrtb @Natural 1 1 1 1) (DrawCtx $ \Paths{..} _ -> mkColor pathsCursor) (collageExtents c)
    activeZone = DrawEmbed outlineRect p

simpleSubreact :: Char -> syn -> Subreact syn
simpleSubreact c syn = do
  KeyPress [Shift] keyCode <- view rctxInputEvent
  guard (keyLetter c keyCode)
  return syn

guardInputEvent :: (InputEvent Int -> Bool) -> React syn
guardInputEvent = guard <=< views rctxInputEvent

class UndoEq a where
  undoEq :: a -> a -> Bool

class SynSelection a sel | a -> sel where
  synSelection :: Lens' a sel
  synSelectionSelf :: Lens' a Bool
