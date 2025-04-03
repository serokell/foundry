{-# LANGUAGE StrictData #-}

module Source.Layout.Cairo.Element
  ( CairoElement(..),
    cairoPositionedElementRender,
    CairoRender(..)
  ) where

import qualified Graphics.Rendering.Cairo as Cairo

import Source.Layout.Inj
import Source.Layout.Core

-- A monadic action to draw a picture.
newtype CairoRender g =
  CairoRender { cairoRender :: (forall x. g x -> x) -> Cairo.Render () }

instance Semigroup (CairoRender g) where
  r1 <> r2 =
    CairoRender $ \getG -> do
      cairoRender r1 getG
      cairoRender r2 getG

instance Monoid (CairoRender g) where
  mempty = CairoRender $ \_ -> return ()

data CairoElement g =
  CairoElement
    { cairoElementExtents :: Extents,
      cairoElementBaseline :: Baseline,
      cairoElementRender :: Offset -> CairoRender g
    }

cairoPositionedElementRender :: Positioned (CairoElement g) -> CairoRender g
cairoPositionedElementRender (At o e) = cairoElementRender e o

instance HasExtents (CairoElement g) where
  extentsOf = cairoElementExtents

instance HasBaseline (CairoElement g) where
  baselineOf = cairoElementBaseline

instance g1 ~ g2 => Inj (CairoElement g1) (CairoElement g2) where
  inj = id
