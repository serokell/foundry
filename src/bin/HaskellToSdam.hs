{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.Split as List
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified EnumSet as GHC.EnumSet
import qualified FastString as GHC
import qualified HsSyn as GHC
import HsSyn (GhcPs)
import qualified Lexer as GHC
import qualified Module as GHC
import qualified OccName as GHC
import qualified Parser as GHC
import qualified RdrName as GHC
import Sdam.Core
import Sdam.Printer
import Source.Language.Haskell
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  et <- getArgs >>= \case
    [et] -> return et
    _ -> die "Usage: haskell-to-sdam EXPR"
  hs_mod <-
    case parseModuleStr et of
      Nothing -> die "Parsing failed"
      Just e -> return e
  putStrLn $ render (rValue (convertModule (GHC.unLoc hs_mod)))

convertModule :: GHC.HsModule GhcPs -> RenderValue
convertModule GHC.HsModule {GHC.hsmodName, GHC.hsmodExports, GHC.hsmodDecls} =
  mkRecValue
    ty_module
    [ ( fld_name,
        case hsmodName of
          Nothing -> mkStrValue ty_v (Text.pack "Main")
          Just name -> convertModuleName (GHC.unLoc name)
      ),
      ( fld_ex,
        case hsmodExports of
          Nothing -> mkRecValue ty_all []
          Just ex -> convertExports (GHC.unLoc ex)
      ),
      (fld_ds, mkSeqValue (convertDecl . GHC.unLoc) hsmodDecls)
    ]

convertModuleName :: GHC.ModuleName -> RenderValue
convertModuleName modname =
  case List.splitOn "." (GHC.moduleNameString modname) of
    [] -> error "convertModuleName: empty list"
    s : ss -> toQVs s ss
  where
    toQVs s [] = mkStrValue ty_v (Text.pack s)
    toQVs q (s : ss) =
      mkRecValue
        ty_qv
        [ (fld_q, mkStrValue ty_v (Text.pack q)),
          (fld_v, toQVs s ss)
        ]

convertExports :: [GHC.LIE GhcPs] -> RenderValue
convertExports = mkSeqValue (convertIE . GHC.unLoc)

convertIE :: GHC.IE GhcPs -> RenderValue
convertIE _ = error "TODO: convertIE"

convertDecl :: GHC.HsDecl GhcPs -> RenderValue
convertDecl (GHC.SigD _ (GHC.TypeSig _ names ty)) =
  convertTypeSig
    (map GHC.unLoc names)
    (GHC.unLoc (GHC.hsib_body (GHC.hswc_body ty)))
convertDecl (GHC.ValD _ GHC.FunBind {GHC.fun_id, GHC.fun_matches}) =
  convertFunBind
    (GHC.unLoc fun_id)
    fun_matches
convertDecl _ = error "TODO: convertDecl"

convertFunBind :: GHC.IdP GhcPs -> GHC.MatchGroup GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertFunBind name GHC.MG {GHC.mg_alts} =
  mkSeqValue (convertMatch name . GHC.unLoc) (GHC.unLoc mg_alts)
convertFunBind _ _ = error "TODO: convertFunBind"

convertMatch :: GHC.IdP GhcPs -> GHC.Match GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertMatch name GHC.Match {GHC.m_pats, GHC.m_grhss} =
  mkRecValue
    ty_bind
    [ (fld_v, toApps name m_pats),
      (fld_b, convertGRHSs m_grhss)
    ]
  where
    toApps n ps =
      foldl
        (\f p -> mkRecValue ty_a [(fld_f, f), (fld_a, p)])
        (convertName n)
        (map convertPat ps)
convertMatch _ _ = error "TODO: convertMatch"

convertPat :: GHC.Pat GhcPs -> RenderValue
convertPat _ = error "TODO: convertPat"

convertGRHSs :: GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertGRHSs GHC.GRHSs {GHC.grhssGRHSs} =
  mkSeqValue (convertGRHS . GHC.unLoc) grhssGRHSs
convertGRHSs _ = error "TODO: convertGRHSs"

convertGRHS :: GHC.GRHS GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertGRHS (GHC.GRHS _ [] b) = convertExpr (GHC.unLoc b)
convertGRHS _ = error "TODO: convertGRHS"

convertTypeSig :: [GHC.IdP GhcPs] -> GHC.HsType GhcPs -> RenderValue
convertTypeSig names ty =
  mkRecValue
    ty_sig
    [ (fld_v, mkSeqValue convertName names),
      (fld_t, convertType ty)
    ]

convertName :: GHC.IdP GhcPs -> RenderValue
convertName name =
  mkStrValue ty_v (Text.pack (GHC.occNameString (GHC.rdrNameOcc name)))

convertType :: GHC.HsType GhcPs -> RenderValue
convertType (GHC.HsTyVar _ _ name) = convertName (GHC.unLoc name)
convertType (GHC.HsAppTy _ t1 t2) =
  mkRecValue
    ty_a
    [ (fld_f, convertType (GHC.unLoc t1)),
      (fld_a, convertType (GHC.unLoc t2))
    ]
convertType (GHC.HsTupleTy _ GHC.HsBoxedOrConstraintTuple []) =
  -- TODO: Unit representation
  mkStrValue ty_v (Text.pack "Unit")
convertType _ = error "TODO: convertType"

convertExpr :: GHC.HsExpr GhcPs -> RenderValue
convertExpr (GHC.HsVar _ name) = convertName (GHC.unLoc name)
convertExpr (GHC.HsApp _ e1 e2) =
  mkRecValue
    ty_a
    [ (fld_f, convertExpr (GHC.unLoc e1)),
      (fld_a, convertExpr (GHC.unLoc e2))
    ]
convertExpr (GHC.HsLit _ lit) =
  convertLit lit
convertExpr _ = error "TODO: convertExpr"

convertLit :: GHC.HsLit GhcPs -> RenderValue
convertLit (GHC.HsString _ s) =
  mkStrValue ty_str (Text.pack (GHC.unpackFS s))
convertLit _ = error "TODO: convertLit"

mkRecValue :: TyName -> [(FieldName, RenderValue)] -> RenderValue
mkRecValue tyName fields =
  RenderValue (ValueRec tyName (HashMap.fromList fields))

mkStrValue :: TyName -> Text -> RenderValue
mkStrValue tyName str =
  RenderValue (ValueStr tyName str)

mkSeqValue :: Foldable f => (a -> RenderValue) -> f a -> RenderValue
mkSeqValue f c =
  case toList c of
    [x] -> f x
    xs -> RenderValue (ValueSeq (Seq.fromList (map f xs)))

parseModuleStr :: String -> Maybe (GHC.Located (GHC.HsModule GhcPs))
parseModuleStr = runGhcParser GHC.parseModule

runGhcParser :: GHC.P a -> String -> Maybe a
runGhcParser p s =
  case GHC.unP p initPState of
    GHC.PFailed _ _ _ -> Nothing
    GHC.POk _ a -> Just a
  where
    initPState :: GHC.PState
    initPState = GHC.mkPStatePure flags buffer location
    flags :: GHC.ParserFlags
    flags =
      GHC.mkParserFlags'
        GHC.EnumSet.empty
        GHC.EnumSet.empty
        GHC.mainUnitId
        False
        False
        False
        False
    buffer :: GHC.StringBuffer
    buffer = GHC.stringToStringBuffer s
    location :: GHC.RealSrcLoc
    location = GHC.mkRealSrcLoc (GHC.mkFastString "<haskell-to-sdam>") 1 1
