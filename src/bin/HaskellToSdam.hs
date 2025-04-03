module Main where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

import Sdam.Core
import Sdam.Printer
import System.Environment (getArgs)
import System.Exit (die)

import GHC.Hs (GhcPs)
import qualified GHC.Hs as GHC
import qualified GHC.Parser as GHC
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Data.EnumSet as GHC.EnumSet
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Utils.Error as GHC

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
    "module_exports_/_"
    [ case hsmodName of
        Nothing -> mkStrValue (Text.pack "Main")
        Just name -> convertModuleName (GHC.unLoc name),
      case hsmodExports of
        Nothing -> mkRecValue "∗" []
        Just ex -> convertExports (GHC.unLoc ex),
      mkSeqValue (convertDecl . GHC.unLoc) hsmodDecls
    ]

convertModuleName :: GHC.ModuleName -> RenderValue
convertModuleName modname = mkStrValue (Text.pack (GHC.moduleNameString modname))

convertExports :: [GHC.LIE GhcPs] -> RenderValue
convertExports = mkSeqValue (convertIE . GHC.unLoc)

convertIE :: GHC.IE GhcPs -> RenderValue
convertIE _ = error "TODO: convertIE"

convertDecl :: GHC.HsDecl GhcPs -> RenderValue
convertDecl (GHC.SigD _ (GHC.TypeSig _ names ty)) =
  convertTypeSig
    (map GHC.unLoc names)
    (GHC.unLoc (GHC.sig_body (GHC.unLoc (GHC.hswc_body ty))))
convertDecl (GHC.ValD _ GHC.FunBind {GHC.fun_id, GHC.fun_matches}) =
  convertFunBind
    (GHC.unLoc fun_id)
    fun_matches
convertDecl _ = error "TODO: convertDecl"

convertFunBind :: GHC.IdP GhcPs -> GHC.MatchGroup GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertFunBind name GHC.MG {GHC.mg_alts} =
  mkSeqValue (convertMatch name . GHC.unLoc) (GHC.unLoc mg_alts)

convertMatch :: GHC.IdP GhcPs -> GHC.Match GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertMatch name GHC.Match {GHC.m_pats, GHC.m_grhss} =
  mkRecValue
    "_=_"
    [ toApps name m_pats,
      convertGRHSs m_grhss
    ]
  where
    toApps n ps =
      foldl
        (\f p -> mkRecValue "__" [f, p])
        (convertName n)
        (map (convertPat . GHC.unLoc) ps)

convertPat :: GHC.Pat GhcPs -> RenderValue
convertPat _ = error "TODO: convertPat"

convertGRHSs :: GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertGRHSs GHC.GRHSs {GHC.grhssGRHSs} =
  mkSeqValue (convertGRHS . GHC.unLoc) grhssGRHSs

convertGRHS :: GHC.GRHS GhcPs (GHC.LHsExpr GhcPs) -> RenderValue
convertGRHS (GHC.GRHS _ [] b) = convertExpr (GHC.unLoc b)
convertGRHS _ = error "TODO: convertGRHS"

convertTypeSig :: [GHC.IdP GhcPs] -> GHC.HsType GhcPs -> RenderValue
convertTypeSig names ty =
  mkRecValue
    "_::_"
    [ mkSeqValue convertName names,
      convertType ty
    ]

convertName :: GHC.IdP GhcPs -> RenderValue
convertName name =
  mkStrValue (Text.pack (GHC.occNameString (GHC.rdrNameOcc name)))

convertType :: GHC.HsType GhcPs -> RenderValue
convertType (GHC.HsTyVar _ _ name) = convertName (GHC.unLoc name)
convertType (GHC.HsAppTy _ t1 t2) =
  mkRecValue
    "__"
    [ convertType (GHC.unLoc t1),
      convertType (GHC.unLoc t2)
    ]
convertType (GHC.HsTupleTy _ GHC.HsBoxedOrConstraintTuple []) =
  mkRecValue "()" []
convertType _ = error "TODO: convertType"

convertExpr :: GHC.HsExpr GhcPs -> RenderValue
convertExpr (GHC.HsVar _ name) = convertName (GHC.unLoc name)
convertExpr (GHC.HsApp _ e1 e2) =
  mkRecValue
    "__"
    [ convertExpr (GHC.unLoc e1),
      convertExpr (GHC.unLoc e2)
    ]
convertExpr (GHC.HsLit _ lit) =
  convertLit lit
convertExpr _ = error "TODO: convertExpr"

convertLit :: GHC.HsLit GhcPs -> RenderValue
convertLit (GHC.HsString _ s) =
  mkStrValue (Text.pack ('"' : GHC.unpackFS s))
convertLit _ = error "TODO: convertLit"

mkRecValue :: SynShape -> [RenderValue] -> RenderValue
mkRecValue shape fields = RenderValue (synReconstruct shape fields)

mkStrValue :: Text -> RenderValue
mkStrValue = RenderValue . Syn . Seq.fromList . map TokenChar . Text.unpack

mkSeqValue :: Foldable f => (a -> RenderValue) -> f a -> RenderValue
mkSeqValue f c =
  case toList c of
    [x] -> f x
    xs ->
      RenderValue . Syn . Seq.fromList $
        TokenChar '|' : map (TokenNode . f) xs

parseModuleStr :: String -> Maybe (GHC.Located (GHC.HsModule GhcPs))
parseModuleStr = runGhcParser GHC.parseModule

runGhcParser :: GHC.P a -> String -> Maybe a
runGhcParser p s =
  case GHC.unP p initPState of
    GHC.PFailed _ -> Nothing
    GHC.POk _ a -> Just a
  where
    initPState :: GHC.PState
    initPState = GHC.initParserState opts buffer location
    opts :: GHC.ParserOpts
    opts = GHC.mkParserOpts GHC.EnumSet.empty (GHC.DiagOpts GHC.EnumSet.empty GHC.EnumSet.empty False False Nothing GHC.defaultSDocContext) [] False False False False
    buffer :: GHC.StringBuffer
    buffer = GHC.stringToStringBuffer s
    location :: GHC.RealSrcLoc
    location = GHC.mkRealSrcLoc (GHC.mkFastString "<haskell-to-sdam>") 1 1
