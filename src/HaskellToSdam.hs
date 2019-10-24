{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import System.Exit (die)
import System.Environment (getArgs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List.Split as List
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import Sdam.Core
import Sdam.Printer

import qualified Module as GHC
import qualified StringBuffer as GHC
import qualified FastString as GHC
import qualified EnumSet as GHC.EnumSet
import qualified SrcLoc as GHC
import qualified HsSyn as GHC
import qualified Lexer as GHC
import qualified Parser as GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import HsSyn (GhcPs)

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
convertModule GHC.HsModule{GHC.hsmodName, GHC.hsmodExports, GHC.hsmodDecls} =
  mkRecValue "module" [
    ("name", case hsmodName of
               Nothing -> mkStrValue "v" "Main"
               Just name -> convertModuleName (GHC.unLoc name)),
    ("ex", case hsmodExports of
             Nothing -> mkRecValue "all" []
             Just ex -> convertExports (GHC.unLoc ex)),
    ("ds", mkSeqValue (convertDecl . GHC.unLoc) hsmodDecls)
  ]

convertModuleName :: GHC.ModuleName -> RenderValue
convertModuleName modname =
  case List.splitOn "." (GHC.moduleNameString modname) of
    [] -> error "convertModuleName: empty list"
    s:ss -> toQVs s ss
  where
    toQVs s [] = mkStrValue "v" (Text.pack s)
    toQVs q (s:ss) =
      mkRecValue "qv" [
        ("q", mkStrValue "v" (Text.pack q)),
        ("v", toQVs s ss)
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
convertDecl (GHC.ValD _ GHC.FunBind{GHC.fun_id, GHC.fun_matches}) =
  convertFunBind
    (GHC.unLoc fun_id)
    fun_matches
convertDecl _ = error "TODO: convertDecl"

convertFunBind :: GHC.IdP GhcPs -> GHC.MatchGroup GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertFunBind name GHC.MG{GHC.mg_alts} =
  mkSeqValue (convertMatch name . GHC.unLoc) (GHC.unLoc mg_alts)
convertFunBind _ _ = error "TODO: convertFunBind"

convertMatch :: GHC.IdP GhcPs -> GHC.Match GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertMatch name GHC.Match{GHC.m_pats, GHC.m_grhss} =
    mkRecValue "bind" [
      ("v", toApps name m_pats),
      ("b", convertGRHSs m_grhss)
    ]
  where
    toApps n ps =
      foldl (\f p -> mkRecValue "a" [("f", f), ("a", p)])
        (convertName n)
        (map convertPat ps)
convertMatch _ _ = error "TODO: convertMatch"

convertPat :: GHC.Pat GhcPs -> RenderValue
convertPat _ = error "TODO: convertPat"

convertGRHSs :: GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertGRHSs GHC.GRHSs{GHC.grhssGRHSs} =
  mkSeqValue (convertGRHS . GHC.unLoc) grhssGRHSs
convertGRHSs _ = error "TODO: convertGRHSs"

convertGRHS :: GHC.GRHS GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertGRHS (GHC.GRHS _ [] b) = convertExpr (GHC.unLoc b)
convertGRHS _ = error "TODO: convertGRHS"

convertTypeSig :: [GHC.IdP GhcPs] -> GHC.HsType GhcPs -> RenderValue
convertTypeSig names ty =
  mkRecValue "sig" [
    ("v", mkSeqValue convertName names),
    ("t", convertType ty)
  ]

convertName :: GHC.IdP GhcPs -> RenderValue
convertName name =
  mkStrValue "v" (Text.pack (GHC.occNameString (GHC.rdrNameOcc name)))

convertType :: GHC.HsType GhcPs -> RenderValue
convertType (GHC.HsTyVar _ _ name) = convertName (GHC.unLoc name)
convertType (GHC.HsAppTy _ t1 t2) =
  mkRecValue "a" [
    ("f", convertType (GHC.unLoc t1)),
    ("a", convertType (GHC.unLoc t2))
  ]
convertType (GHC.HsTupleTy _ GHC.HsBoxedOrConstraintTuple []) =
  -- TODO: Unit representation
  mkStrValue "v" "Unit"
convertType _ = error "TODO: convertType"

convertExpr :: GHC.HsExpr GhcPs -> RenderValue
convertExpr (GHC.HsVar _ name) = convertName (GHC.unLoc name)
convertExpr (GHC.HsApp _ e1 e2) =
  mkRecValue "a" [
    ("f", convertExpr (GHC.unLoc e1)),
    ("a", convertExpr (GHC.unLoc e2))
  ]
convertExpr (GHC.HsLit _ lit) =
  convertLit lit
convertExpr _ = error "TODO: convertExpr"

convertLit :: GHC.HsLit GhcPs -> RenderValue
convertLit (GHC.HsString _ s) =
  mkStrValue "str" (Text.pack (GHC.unpackFS s))
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
