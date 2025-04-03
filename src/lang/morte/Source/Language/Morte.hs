{-# LANGUAGE OverloadedLists #-}

module Source.Language.Morte where

import Data.HashMap.Strict (HashMap)
import Data.Primitive.Array (Array)
import Sdam.Core
import Source.Plugin

mortePlugin :: Plugin
mortePlugin =
  Plugin
    { pluginSchema = morteSchema,
      pluginPrecInfo = mortePrecInfo,
      pluginShapeNames = morteShapeNames
    }

morteShapeNames :: HashMap SynShape ShapeName
morteShapeNames =
  [ "__" ==> ShapeName "function application" ["function", "argument"],
    "λ_:_/_" ==> ShapeName "lambda function" ["variable", "type", "body"],
    "Π_:_/_" ==> ShapeName "pi-type" ["variable", "type", "body"],
    "_@_" ==> ShapeName "indexed variable" ["variable", "index"]
  ]

mortePrecInfo :: HashMap SynShape (Array PrecPredicate)
mortePrecInfo =
  [ "__"
      ==> [ precAllow (precAtoms <> ["__"]),
            precAllow precAtoms
          ],
    "λ_:_/_" ==> [noPrec, precAllowAll, precAllowAll],
    "Π_:_/_" ==> [noPrec, precAllowAll, precAllowAll]
  ]
  where
    precAtoms = ["★", "□"]

morteSchema :: Schema
morteSchema =
  Schema
    { schemaShapes =
        [ "_@_",
          "λ_:_/_",
          "Π_:_/_",
          "__",
          "★",
          "□"
        ],
      schemaRoot = tExpr,
      schemaSeqGuard = '|'
    }
  where
    tNat = mempty -- strings only
    tVar = mempty -- strings only
    tIVar = mkTyUnion "_@_" [tVar, tNat]
    tLam = mkTyUnion "λ_:_/_" [tVar, tExpr, tExpr]
    tPi = mkTyUnion "Π_:_/_" [tVar, tExpr, tExpr]
    tApp = mkTyUnion "__" [tExpr, tExpr]
    tStar = mkTyUnion "★" []
    tBox = mkTyUnion "□" []
    tExpr =
      mconcat
        [ tLam,
          tPi,
          tApp,
          tStar,
          tBox,
          tVar,
          tIVar
        ]

--------------------------------------------------------------------------------
---- Helpers
--------------------------------------------------------------------------------

(==>) :: a -> b -> (a, b)
(==>) = (,)

infix 0 ==>
