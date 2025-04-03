module Sdam.Printer
  (
    -- Value
    rValue,
    RenderValue(..),

    -- Path
    rPath,

    -- Running
    render
  ) where

import Prelude hiding ((<>))

import Data.List
import Text.PrettyPrint

import Sdam.Core

rSynShape :: SynShape -> Doc
rSynShape = text . concatMap escape . flattenSynShape
  where
    escape '\n' = "\\n"
    escape c
      | needsEscape c = ['\\', c]
      | otherwise = [c]
    needsEscape c =
      c `elem` ("\\\n " :: [Char])

newtype RenderValue = RenderValue (Syn RenderValue)
  deriving newtype Show

rValue :: RenderValue -> Doc
rValue (RenderValue syn) =
  hang (rSynShape (synShape syn)) 2 (sep (map rValue (synFields syn)))

rPath :: Path -> Doc
rPath (Path ps) =
  hsep $
  intersperse (char '/') $
  map rPathSegment ps

rPathSegment :: PathSegment -> Doc
rPathSegment (PathSegment shape i) =
  rSynShape shape <+> brackets (rIndex i)

rIndex :: Index -> Doc
rIndex = int . indexToInt
