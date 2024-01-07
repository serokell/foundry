{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Graphics.Rendering.Cairo
import Sdam.Parser (pValue, parse)
import Slay.Core
import Source.Language.Haskell
import Source.Language.Morte
import Source.NewGen
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
import Text.Megaparsec as Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  (parsedValue, lang, outputFilepath) <- getArgs >>= \case
    ['-' : '-' : lang, filepath] -> do
      content <- readFile filepath
      case parse pValue filepath content of
        Left e -> die (Megaparsec.errorBundlePretty e)
        Right a -> return (a, lang, replaceExtension filepath "svg")
    _ -> die "Usage: sdam-to-svg --haskell FILE.sd"
  plugin <-
    case lang of
      "haskell" -> return haskellPlugin
      "morte" -> return mortePlugin
      _ -> die "Unsupported language."
  let lctx :: LayoutCtx
      lctx =
        LayoutCtx
          { _lctxPath = mempty @PathBuilder,
            _lctxValidationResult = mempty,
            _lctxViewport = Extents 0 0,
            _lctxPrecBordersAlways = False,
            _lctxPrecInfo = pluginPrecInfo plugin,
            _lctxShapeNames = pluginShapeNames plugin,
            _lctxPlaceholder = Nothing,
            _lctxPrecPredicate = precAllowAll,
            _lctxWritingDirection = WritingDirectionLTR
          }
      exprCollage :: Collage () El
      exprCollage = layoutNodeStandalone lctx (fromParsedValue parsedValue)
      exprExtents = extentsOf exprCollage
      dim f = fromIntegral (f exprExtents)
  withSVGSurface outputFilepath (dim extentsW) (dim extentsH) $ \surface ->
    renderWith surface $ do
      cairoRender
        (getNoAnn $ foldCairoCollage offsetZero exprCollage)
        ($ defaultDrawCtx)
