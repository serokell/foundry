{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Source.Language.Morte where

import Control.Monad (void)
import Data.Char as Char
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HashMap
import Data.List as List
import Sdam.Core
import Source.Layout
import Text.Regex.Applicative as RE

--------------------------------------------------------------------------------
---- Layout
--------------------------------------------------------------------------------

morteRecLayouts :: HashMap TyName ALayoutFn
morteRecLayouts = recLayouts
  where
    recLayouts =
      [ ty_lam ==> recLayoutLam,
        ty_pi ==> recLayoutPi,
        ty_a ==> recLayoutApp,
        ty_star ==> "★",
        ty_box ==> "□",
        ty_iv ==> recLayoutIVar
      ]
    precAll = precAllow (HashMap.keysSet recLayouts)
    precAtoms = [ty_star, ty_box]
    prec ss = precAllow (ss <> precAtoms)
    recLayoutApp =
      field fld_fn (prec [ty_a]) "function"
        <> field fld_arg (prec []) "argument"
    recLayoutLam =
      "λ" <> field fld_var noPrec "variable" <> ":" <> field fld_ty precAll "type"
        `vsep` field "body" precAll "body"
    recLayoutPi =
      "Π" <> field fld_var noPrec "variable" <> ":" <> field fld_ty precAll "type"
        `vsep` field "body" precAll "body"
    recLayoutIVar =
      field fld_var noPrec "variable" <> "@" <> field fld_index noPrec "index"

--------------------------------------------------------------------------------
---- Schema
--------------------------------------------------------------------------------

morteSchema :: Schema
morteSchema =
  Schema
    { schemaTypes =
        [ ty_nat ==> TyDefnStr,
          ty_v ==> TyDefnStr, -- variable
          ty_iv ==> TyDefnRec [fld_var, fld_index], -- indexed variable
          ty_lam ==> TyDefnRec [fld_var, fld_ty, fld_body],
          ty_pi ==> TyDefnRec [fld_var, fld_ty, fld_body],
          ty_a ==> TyDefnRec [fld_fn, fld_arg], -- function application
          ty_star ==> TyDefnRec [],
          ty_box ==> TyDefnRec []
        ],
      schemaRoot = tExpr
    }
  where
    tNat =
      tyUnionSingleton ty_nat $
        TyInstStr (void re)
      where
        re = RE.some (RE.psym Char.isDigit)
    tVar =
      tyUnionSingleton ty_v $
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
    tIVar =
      tyUnionSingleton ty_iv $
        TyInstRec
          [ fld_var ==> tVar,
            fld_index ==> tNat
          ]
    tLam =
      tyUnionSingleton ty_lam $
        TyInstRec
          [ fld_var ==> tVar,
            fld_ty ==> tExpr,
            fld_body ==> tExpr
          ]
    tPi =
      tyUnionSingleton ty_pi $
        TyInstRec
          [ fld_var ==> tVar,
            fld_ty ==> tExpr,
            fld_body ==> tExpr
          ]
    tApp =
      tyUnionSingleton ty_a $
        TyInstRec
          [ fld_fn ==> tExpr,
            fld_arg ==> tExpr
          ]
    tStar =
      tyUnionSingleton ty_star $
        TyInstRec []
    tBox =
      tyUnionSingleton ty_box $
        TyInstRec []
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

--------------------------------------------------------------------------------
---- Type Names
--------------------------------------------------------------------------------

ty_nat :: TyName
ty_nat = "nat"

ty_v :: TyName
ty_v = "v"

ty_iv :: TyName
ty_iv = "iv"

ty_lam :: TyName
ty_lam = "lam"

ty_pi :: TyName
ty_pi = "pi"

ty_a :: TyName
ty_a = "a"

ty_star :: TyName
ty_star = "star"

ty_box :: TyName
ty_box = "box"

--------------------------------------------------------------------------------
---- Field Names
--------------------------------------------------------------------------------

fld_var :: FieldName
fld_var = "var"

fld_index :: FieldName
fld_index = "index"

fld_ty :: FieldName
fld_ty = "ty"

fld_body :: FieldName
fld_body = "body"

fld_fn :: FieldName
fld_fn = "fn"

fld_arg :: FieldName
fld_arg = "arg"
