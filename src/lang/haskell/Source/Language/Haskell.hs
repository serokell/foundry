{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Source.Language.Haskell where

import Control.Monad (void)
import Data.Char as Char
import Data.HashMap.Strict (HashMap)
import Data.List as List
import Sdam.Core
import Source.Layout
import Text.Regex.Applicative as RE

--------------------------------------------------------------------------------
---- Layout
--------------------------------------------------------------------------------

haskellRecLayouts :: HashMap TyName ALayoutFn
haskellRecLayouts = recLayouts
  where
    recLayouts =
      [ ty_all ==> recLayoutAll,
        ty_lam ==> recLayoutLam,
        ty_a ==> recLayoutApp,
        ty_module ==> recLayoutMod,
        ty_qv ==> recLayoutQVar,
        ty_sig ==> recLayoutSig,
        ty_as_pat ==> recLayoutAsPat,
        ty_bind ==> recLayoutBind,
        ty_data ==> recLayoutData,
        ty_import ==> recLayoutImport,
        ty_qualified ==> recLayoutQualified,
        ty_as_mod ==> recLayoutAsMod
      ]
    recLayoutAll = jumptag "∗"
    recLayoutQVar =
      field fld_q noPrec "q" <> "." <> field fld_v precAllowAll "v"
    recLayoutApp =
      field fld_f (precAllow [ty_a]) "function"
        <> field fld_a (precAllow [ty_v, ty_qv]) "argument"
    recLayoutLam =
      jumptag "λ" <> field fld_v precAllowAll "variable"
        `vsep` field fld_b precAllowAll "body"
    recLayoutMod =
      jumptag "module" <> field fld_name (precAllow ["v", "qv"]) "name"
        <> "exports"
        <> field fld_ex precAllowAll "entities"
        `vsep` field fld_ds precAllowAll "declarations"
    recLayoutSig =
      field fld_v noPrec "variable" <> jumptag "::"
        <> field fld_t precAllowAll "type"
    recLayoutAsPat =
      field fld_alias noPrec "alias" <> jumptag "@"
        <> field fld_p noPrec "pattern"
    recLayoutBind =
      field fld_v noPrec "variable" <> jumptag "="
        <> field fld_b precAllowAll "body"
    recLayoutData =
      jumptag "data" <> field fld_v noPrec "name" <> "="
        <> field fld_alts precAllowAll "alternatives"
    recLayoutImport =
      jumptag "from" <> field fld_module (precAllow [ty_v, ty_qv, ty_as_mod]) "module"
        <> "import"
        <> field fld_e precAllowAll "entities"
    recLayoutQualified =
      jumptag "qualified" <> field fld_entities (precAllow [ty_v]) "entities"
    recLayoutAsMod =
      field fld_module noPrec "module" <> jumptag "as"
        <> field fld_alias noPrec "alias"

--------------------------------------------------------------------------------
---- Schema
--------------------------------------------------------------------------------

haskellSchema :: Schema
haskellSchema =
  Schema
    { schemaTypes =
        [ ty_all ==> TyDefnRec [],
          ty_module ==> TyDefnRec [fld_name, fld_ex, fld_ds],
          ty_v ==> TyDefnStr,
          ty_str ==> TyDefnStr,
          ty_lam ==> TyDefnRec [fld_v, fld_b],
          ty_a ==> TyDefnRec [fld_f, fld_a],
          ty_qv ==> TyDefnRec [fld_q, fld_v],
          ty_sig ==> TyDefnRec [fld_v, fld_t],
          ty_as_pat ==> TyDefnRec [fld_alias, fld_p],
          ty_bind ==> TyDefnRec [fld_v, fld_b],
          ty_data ==> TyDefnRec [fld_v, fld_alts],
          ty_import ==> TyDefnRec [fld_module, fld_e],
          ty_qualified ==> TyDefnRec [fld_entities],
          ty_as_mod ==> TyDefnRec [fld_module, fld_alias]
        ],
      schemaRoot = tMod
    }
  where
    tVar =
      uT ty_v $
        TyInstStr (void re)
      where
        re = re_alphavar <|> re_op
        re_fst =
          RE.psym $ \c ->
            Char.isLetter c
              || c == '_'
        re_labelchar =
          RE.psym $ \c ->
            Char.isLetter c
              || Char.isDigit c
              || c == '_'
        re_opchar =
          RE.psym $ \c ->
            c `List.elem` ("!#$%&*+./<=>?@^|-~" :: [Char])
        re_alphavar =
          re_fst *> RE.many re_labelchar
        re_op =
          RE.some re_opchar
    tStr =
      uT ty_str $
        TyInstStr (void (RE.many RE.anySym))
    tQVar =
      uT ty_qv $
        TyInstRec
          [ fld_q ==> tVar,
            fld_v ==> tVar <> tQVar
          ]
    tAll =
      uT ty_all $
        TyInstRec []
    tMod =
      uT ty_module $
        TyInstRec
          [ fld_name ==> tVar <> tQVar,
            fld_ex ==> tAll <> uS' tVar,
            fld_ds ==> uS' tDecl
          ]
    tLam =
      uT ty_lam $
        TyInstRec
          [ fld_v ==> tVar,
            fld_b ==> tExpr
          ]
    tExprApp =
      uT ty_a $
        TyInstRec
          [ fld_f ==> tExpr,
            fld_a ==> tExpr
          ]
    tPatApp =
      uT ty_a $
        TyInstRec
          [ fld_f ==> tPat,
            fld_a ==> tPat
          ]
    tTypeApp =
      uT ty_a $
        TyInstRec
          [ fld_f ==> tType,
            fld_a ==> tType
          ]
    tDeclSig =
      uT ty_sig $
        TyInstRec
          [ fld_v ==> tVar <> uS tVar,
            fld_t ==> tType
          ]
    tExprSig =
      uT ty_sig $
        TyInstRec
          [ fld_v ==> tExpr,
            fld_t ==> tType
          ]
    tPatSig =
      uT ty_sig $
        TyInstRec
          [ fld_v ==> tPat,
            fld_t ==> tType
          ]
    tTypeSig =
      uT ty_sig $
        TyInstRec
          [ fld_v ==> tType,
            fld_t ==> tKind
          ]
    tBind =
      uT ty_bind $
        TyInstRec
          [ fld_v ==> tPat,
            fld_b ==> tExpr
          ]
    tData =
      uT ty_data $
        TyInstRec
          [ fld_v ==> tVar,
            fld_alts ==> uS tExpr
          ]
    tImport =
      uT ty_import $
        TyInstRec
          [ fld_module ==> tVar <> tQVar <> tAsMod,
            fld_e ==> tAll <> uS' tVar <> tQualified
          ]
    tAsMod =
      uT ty_as_mod $
        TyInstRec
          [ fld_module ==> tVar <> tQVar,
            fld_alias ==> tVar <> tQVar
          ]
    tQualified =
      uT ty_qualified $
        TyInstRec
          [ fld_entities ==> tAll <> uS' tVar
          ]
    tAsPat =
      uT ty_as_pat $
        TyInstRec
          [ fld_alias ==> tVar,
            fld_p ==> tPat
          ]
    tExpr =
      mconcat
        [ tLam,
          tExprApp,
          tStr,
          tVar,
          tQVar,
          tExprSig
        ]
    tKind = tType
    tType =
      mconcat
        [ tVar,
          tQVar,
          tTypeApp,
          tTypeSig
          -- tForall
        ]
    tPat =
      mconcat
        [ tVar,
          tQVar,
          tPatApp,
          tPatSig,
          tAsPat
        ]
    tDecl =
      mconcat
        [ tDeclSig,
          tBind,
          tData,
          tImport
        ]

--------------------------------------------------------------------------------
---- Helpers
--------------------------------------------------------------------------------

uT :: TyName -> TyInst -> TyUnion
uT = tyUnionSingleton

uS :: TyUnion -> TyUnion
uS = tyUnionSequence

uS' :: TyUnion -> TyUnion
uS' = tyUnionRecursiveSequence

(==>) :: a -> b -> (a, b)
(==>) = (,)

infix 0 ==>

--------------------------------------------------------------------------------
---- Type Names
--------------------------------------------------------------------------------

ty_all :: TyName
ty_all = "all"

ty_module :: TyName
ty_module = "module"

ty_v :: TyName
ty_v = "v"

ty_str :: TyName
ty_str = "str"

ty_lam :: TyName
ty_lam = "lam"

ty_a :: TyName
ty_a = "a"

ty_qv :: TyName
ty_qv = "qv"

ty_sig :: TyName
ty_sig = "sig"

ty_as_pat :: TyName
ty_as_pat = "as-pat"

ty_bind :: TyName
ty_bind = "bind"

ty_data :: TyName
ty_data = "data"

ty_import :: TyName
ty_import = "import"

ty_qualified :: TyName
ty_qualified = "qualified"

ty_as_mod :: TyName
ty_as_mod = "as-mod"

--------------------------------------------------------------------------------
---- Field Names
--------------------------------------------------------------------------------

fld_name :: FieldName
fld_name = "name"

fld_ex :: FieldName
fld_ex = "ex"

fld_ds :: FieldName
fld_ds = "ds"

fld_v :: FieldName
fld_v = "v"

fld_b :: FieldName
fld_b = "b"

fld_f :: FieldName
fld_f = "f"

fld_a :: FieldName
fld_a = "a"

fld_q :: FieldName
fld_q = "q"

fld_t :: FieldName
fld_t = "t"

fld_alias :: FieldName
fld_alias = "alias"

fld_p :: FieldName
fld_p = "p"

fld_alts :: FieldName
fld_alts = "alts"

fld_module :: FieldName
fld_module = "module"

fld_e :: FieldName
fld_e = "e"

fld_entities :: FieldName
fld_entities = "entities"
