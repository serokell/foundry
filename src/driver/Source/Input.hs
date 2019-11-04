module Source.Input
  ( Modifier (..),
    KeyCode,
    keyChar,
    InputEvent (..),
  )
where

import Data.Word
import qualified Graphics.UI.Gtk as Gtk

data Modifier = Control | Shift | Alt
  deriving (Eq, Show)

type KeyCode = Word32

keyChar :: KeyCode -> Maybe Char
keyChar = Gtk.keyToChar

data InputEvent
  = KeyPress [Modifier] KeyCode
  | KeyRelease [Modifier] KeyCode
  | PointerMotion Int Int
  | ButtonPress
  deriving (Eq, Show)
