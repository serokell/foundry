module Source.Layout.Cairo.Prim.Color
  ( Color(..),
    rgb,
    grayscale,
    setSourceColor
  ) where

import Data.Word
import Data.Hashable
import GHC.Generics (Generic)

import qualified Graphics.Rendering.Cairo as Cairo

import Source.Layout.Inj

data Color =
  -- true color (24bit)
  RGB Word8 Word8 Word8
  deriving (Eq, Ord, Show, Generic)

instance Hashable Color

instance p ~ Color => Inj p Color

rgb :: Inj Color a => Word8 -> Word8 -> Word8 -> a
rgb r g b = inj (RGB r g b)

grayscale :: Inj Color a => Word8 -> a
grayscale a = rgb a a a

setSourceColor :: Color -> Cairo.Render ()
setSourceColor (RGB r g b) =
  Cairo.setSourceRGB (toDouble r) (toDouble g) (toDouble b)
  where
    toDouble x = fromIntegral x / fromIntegral (maxBound :: Word8)
