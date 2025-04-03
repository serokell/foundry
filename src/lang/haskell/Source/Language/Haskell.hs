{-# LANGUAGE OverloadedLists #-}

module Source.Language.Haskell where

import Data.HashMap.Strict (HashMap)
import Data.Primitive.Array (Array)
import Source.Plugin

haskellPlugin :: Plugin
haskellPlugin =
  Plugin
    { pluginSchema = haskellSchema,
      pluginPrecInfo = haskellPrecInfo,
      pluginShapeNames = haskellShapeNames
    }

haskellShapeNames :: HashMap SynShape ShapeName
haskellShapeNames =
  [ "__" ==> ShapeName "function application" ["function", "argument"],
    "_→_" ==> ShapeName "function type" ["domain", "codomain"],
    "λ_/_" ==> ShapeName "lambda function" ["pattern", "body"],
    "module_exports_/_" ==> ShapeName "module header" ["name", "entities", "declarations"],
    "_::_" ==> ShapeName "type annotation" ["left-hand side", "right-hand side"],
    "_@_" ==> ShapeName "as-pattern" ["alias", "pattern"],
    "_=_" ==> ShapeName "binding" ["left-hand side", "right-hand side"],
    "data_=_" ==> ShapeName "data declaration" ["header", "alternatives"],
    "newtype_=_" ==> ShapeName "newtype declaration" ["header", "alternatives"],
    "from_import_" ==> ShapeName "import" ["module", "entities"],
    "from_import_qualified" ==> ShapeName "import" ["module", "entities"],
    "from_as_import_" ==> ShapeName "import" ["module", "alias", "entities"],
    "from_as_import_qualified" ==> ShapeName "import" ["module", "alias", "entities"],
    "∗" ==> ShapeName "all" []
  ]

haskellPrecInfo :: HashMap SynShape (Array PrecPredicate)
haskellPrecInfo =
  [ "__" ==> [precAllow ["__"], noPrec],
    "λ_/_" ==> [precAllowAll, precAllowAll],
    "from_import_" ==> [noPrec, precAllowAll],
    "from_import_qualified" ==> [noPrec, precAllowAll],
    "from_as_import_" ==> [noPrec, precAllowAll, precAllowAll],
    "from_as_import_qualified" ==> [noPrec, precAllowAll, precAllowAll],
    "module_exports_/_" ==> [noPrec, precAllowAll, precAllowAll],
    "_::_" ==> [noPrec, precAllowAll],
    "_=_" ==> [noPrec, precAllowAll],
    "data_=_" ==> [noPrec, precAllowAll],
    "newtype_=_" ==> [noPrec, precAllowAll]
  ]

haskellSchema :: Schema
haskellSchema =
  Schema
    { schemaShapes =
        [ "__",
          "_→_",
          "λ_/_",
          "module_exports_/_",
          "_::_",
          "_@_",
          "_=_",
          "data_=_",
          "newtype_=_",
          "from_import_",
          "from_import_qualified",
          "from_as_import_",
          "from_as_import_qualified",
          "∗"
        ],
      schemaRoot = tMod,
      schemaSeqGuard = '|'
    }
  where
    tVar = mempty -- strings only
    tStr = mempty -- strings only
    tAll = mkTyUnion "∗" []
    tMod = mkTyUnion "module_exports_/_" [tVar, tAll <> tVar, tDecl]
    tDecl =
      mconcat
        [ mkTyUnion "_::_" [tVar, tType],
          mkTyUnion "_=_" [tPat, tExpr],
          tData,
          tImport
        ]
    tImport =
      mconcat
        [ mkTyUnion "from_import_" [tVar, tVar],
          mkTyUnion "from_import_qualified" [tVar, tVar],
          mkTyUnion "from_as_import_" [tVar, tVar, tVar],
          mkTyUnion "from_as_import_qualified" [tVar, tVar, tVar]
        ]
    tData =
      mconcat
        [ mkTyUnion "data_=_" [tType, tConDecl],
          mkTyUnion "newtype_=_" [tType, tConDecl]
        ]
    tConDecl =
      mconcat
        [ tVar,
          mkTyUnion "__" [tConDecl, tType]
        ]
    tExpr =
      mconcat
        [ tVar,
          tStr,
          mkTyUnion "__" [tExpr, tExpr],
          mkTyUnion "λ_/_" [tPat, tExpr],
          mkTyUnion "_::_" [tExpr, tType]
        ]
    tType =
      mconcat
        [ tVar,
          tStr,
          mkTyUnion "__" [tExpr, tExpr],
          mkTyUnion "_::_" [tType, tType]
        ]
    tPat =
      mconcat
        [ tVar,
          tStr,
          mkTyUnion "__" [tPat, tPat],
          mkTyUnion "_::_" [tPat, tType]
        ]

--------------------------------------------------------------------------------
---- Helpers
--------------------------------------------------------------------------------

(==>) :: a -> b -> (a, b)
(==>) = (,)

infix 0 ==>
