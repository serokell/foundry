{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Source.NewGen
  (
  -- * Names
  TyName,
  FieldName,

  -- * Types
  Env(..),
  Ty(..),
  TyUnion(..),
  mkTyUnion,

  -- * Identifiers
  TyId,
  mkTyId,
  FieldId,
  mkFieldId,

  -- * Values
  Holey(..),
  Object(..),
  Value(..),

  SynStr(..),
  synStrContent,
  synStrPosition,
  synStrEditMode,

  SelfSel(..),
  RecSel(..),
  SynRec(..),
  synRecFields,
  synRecSel,

  -- * Path
  PathSegment(..),
  Path(..),
  emptyPath,
  PathBuilder,

  -- * Draw
  offsetZero,
  CursorBlink(..),
  blink,
  Paths(..),
  DrawCtx(..),
  withDrawCtx,
  Draw,
  toCairoElementsDraw,
  PrecPredicate,
  precAllow,
  Layout(vsep, field),
  noPrec,
  ALayoutFn(..),
  WritingDirection(..),
  findPath,

  -- * React
  keyLetter,
  keyCodeLetter,
  shiftChar,
  insertModeEvent,

  -- * Editor
  EditorState(..),
  esExpr,
  esPointer,
  esHoverBarEnabled,
  esPrecBordersAlways,
  esWritingDirection,
  esUndo,
  esRedo,

  NodeCreateFn(..),
  ncfCheckInputEvent,
  ncfTyId,

  LayoutCtx(..),
  lctxPath,
  lctxViewport,
  lctxPrecBordersAlways,
  lctxRecLayouts,
  lctxEnvNameInfo,
  lctxWritingDirection,

  ReactCtx(..),
  rctxFindPath,
  rctxInputEvent,
  rctxNodeFactory,
  rctxDefaultValues,
  rctxAllowedFieldTypes,
  rctxRecMoveMaps,
  rctxWritingDirection,

  RecMoveMap,

  layoutEditorState,
  selectionPathEditorState,
  reactEditorState,

  -- * Plugin
  Plugin(..),
  pluginTyEnv,
  pluginRecLayouts,
  pluginNodeFactory,

  PluginInfo(..),
  pluginInfoTyEnv,
  pluginInfoRecLayouts,
  pluginInfoNodeFactory,
  pluginInfoEnvNameInfo,
  pluginInfoRecMoveMaps,
  pluginInfoDefaultValues,
  pluginInfoAllowedFieldTypes,
  mkPluginInfo,

  -- * Utils
  inj,
  nothing,
  maybeA

  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Either
import Data.List as List
import Data.List.NonEmpty as NonEmpty hiding (cons)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens as Lens hiding (elements)
import Data.Monoid
import Data.Foldable
import Data.Function (on)
import Data.String
import Inj
import Inj.Base ()

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Char as Char

import Slay.Core
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Combinators
import Slay.Cairo.Element

import Source.Input
import qualified Source.Input.KeyCode as KeyCode

import Sdam.Core hiding (Value(ValueRec, ValueStr), Object(Object))
import Sdam.NameInfo

mkTyUnion :: [TyName] -> TyUnion
mkTyUnion = TyUnion . Set.fromList

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data Holey a =
  Hole |
  Solid a

data Object =
  Object TyId Value

data Value =
  ValueRec SynRec |
  ValueStr SynStr

data SynStr =
  SynStr
    { _synStrContent :: Text,
      _synStrPosition :: Int,
      _synStrEditMode :: Bool
    }

data SelfSel =
  -- for empty synRecFields
  SelfSelEmpty |
  -- for non-empty synRecFields
  SelfSelChild FieldId

data RecSel =
  RecSelSelf SelfSel |
  RecSelChild FieldId

data SynRec =
  SynRec
    { _synRecFields :: Map FieldId (Holey Object),
      _synRecSel :: RecSel
    }

atPath :: Path -> Holey Object -> Maybe TyId
atPath (Path p0) = goHoleyObject p0
  where
    goHoleyObject _ Hole = Nothing
    goHoleyObject p (Solid a) = goObject p a
    goObject [] (Object tyId _) = Just tyId
    goObject (ps:p) (Object _ v) = goValue ps p v
    goValue (PathSegmentRec fieldId) p (ValueRec r) = goRec fieldId p r
    goValue _ _ _ = Nothing
    goRec fieldId p SynRec{_synRecFields=fields} =
      Map.lookup fieldId fields >>= goHoleyObject p

--------------------------------------------------------------------------------
---- Drawing
--------------------------------------------------------------------------------

data CursorBlink = CursorVisible | CursorInvisible

blink :: CursorBlink -> CursorBlink
blink = \case
  CursorVisible -> CursorInvisible
  CursorInvisible -> CursorVisible

data Paths =
  Paths
    { pathsCursor :: Maybe Path,
      pathsSelection :: Path }

data DrawCtx a = DrawCtx (Paths -> CursorBlink -> a)
  deriving (Functor)

instance Inj p a => Inj p (DrawCtx a) where
  inj p = DrawCtx (\_ _ -> inj p)

withDrawCtx :: Paths -> CursorBlink -> DrawCtx a -> a
withDrawCtx paths curBlink (DrawCtx f) = f paths curBlink

data Draw
  = DrawCairoElement (CairoElement DrawCtx)
  | DrawEmbed (CairoElement DrawCtx) Path

instance HasExtents Draw where
  extentsOf = extentsOf . toCairoElementDraw

instance HasBaseline Draw where
  baselineOf = baselineOf . toCairoElementDraw

toCairoElementDraw :: Draw -> CairoElement DrawCtx
toCairoElementDraw = \case
  DrawCairoElement ce -> ce
  DrawEmbed ce _ -> ce

toCairoElementsDraw :: [Positioned Draw] -> [Positioned (CairoElement DrawCtx)]
toCairoElementsDraw elements =
    -- We want DrawEmbed elements to have a higher z-index,
    -- so we place them after all other elements.
    otherElements ++ embedElements
  where
    toEither (At o el) =
      case el of
        DrawEmbed ce _ -> Left (At o ce)
        DrawCairoElement ce -> Right (At o ce)
    (embedElements, otherElements) =
      partitionEithers (List.map toEither elements)

instance g ~ DrawCtx => Inj (CairoElement g) Draw where
  inj = DrawCairoElement

textline ::
  Inj (CairoElement DrawCtx) a =>
  Color -> Font -> Text -> (Paths -> CursorBlink -> Maybe Natural) -> a
textline color font str cur = text font (inj color) str (DrawCtx cur)

line :: Color -> Natural -> Collage Draw
line color w = rect nothing (inj color) (Extents w 1)

centerOf :: Extents -> Collage Draw -> LRTB Natural
centerOf (Extents vacantWidth vacantHeight) collage =
  let
    Extents width height = collageExtents collage
    (excessWidth1, excessWidth2) = integralDistribExcess vacantWidth width
    (excessHeight1, excessHeight2) = integralDistribExcess vacantHeight height
  in
    LRTB
      { left = excessWidth1,
        right = excessWidth2,
        top = excessHeight1,
        bottom = excessHeight2 }

data WritingDirection = WritingDirectionLTR | WritingDirectionRTL

infixr 1 `vsep`

class (IsString a, Semigroup a) => Layout a where
  vsep :: a -> a -> a
  field :: FieldName -> PrecPredicate -> a

noPrec :: PrecPredicate
noPrec = PrecPredicate (const (PrecBorder True))

newtype RecLayoutFn =
  RecLayoutFn {
    appRecLayoutFn ::
      Map FieldId (PrecPredicate -> (PrecUnenclosed, Collage Draw)) ->
      TyId ->
      WritingDirection ->
      (PrecUnenclosed, Collage Draw)
  }

instance IsString RecLayoutFn where
  fromString s =
    RecLayoutFn $ \_ _ _ ->
      (mempty, punct (fromString s))

instance Semigroup RecLayoutFn where
  RecLayoutFn a <> RecLayoutFn b =
    RecLayoutFn $ \m tyId wd ->
      let
        (aUnenclosed, a') = a m tyId wd
        (bUnenclosed, b') = b m tyId wd
        f = case wd of
          WritingDirectionLTR -> horizBaseline
          WritingDirectionRTL -> flip horizBaseline
      in
        (,) (aUnenclosed <> bUnenclosed) $
        f a' b'

instance Layout RecLayoutFn where
  RecLayoutFn a `vsep` RecLayoutFn b =
    RecLayoutFn $ \m tyId wd ->
      let
        (aUnenclosed, a') = a m tyId wd
        (bUnenclosed, b') = b m tyId wd
        f = case wd of
          WritingDirectionLTR -> vertLeft
          WritingDirectionRTL -> vertRight
        maxWidth = (max `on` widthOf) a' b'
      in
        (,) (aUnenclosed <> bUnenclosed) $
        a' `f` line light1 maxWidth `f` b'
  field fieldName precPredicate =
    RecLayoutFn $ \m tyId _ ->
      (m Map.! mkFieldId' tyId fieldName) precPredicate

newtype ALayoutFn = ALayoutFn (forall a. Layout a => a)

instance IsString ALayoutFn where
  fromString s = ALayoutFn (fromString s)

instance Semigroup ALayoutFn where
  ALayoutFn a <> ALayoutFn b =
    ALayoutFn (a <> b)

instance Layout ALayoutFn where
  ALayoutFn a `vsep` ALayoutFn b =
    ALayoutFn (a `vsep` b)
  field fieldName precPredicate =
    ALayoutFn (field fieldName precPredicate)

findPath ::
  NonEmpty (Positioned Draw) ->
  Offset ->
  Maybe Path
findPath c o =
  getFirst $ foldMap (First . check) c
  where
    check (At o' d) = do
      let e = extentsOf d
      DrawEmbed _ p <- Just d
      guard $ insideBox (o', e) o
      Just p

dark1, dark2, light1, white :: Color
dark1  = RGB 41 41 41
dark2  = RGB 77 77 77
light1 = RGB 179 179 179
white  = RGB 255 255 255

textWithCursor :: Text -> (Paths -> CursorBlink -> Maybe Natural) -> Collage Draw
textWithCursor = textline white ubuntuFont

textWithoutCursor :: Text -> Collage Draw
textWithoutCursor t =
  textWithCursor t (\_ _ -> Nothing)

outline ::
  Inj (CairoElement DrawCtx) a =>
  Natural -> DrawCtx (Maybe Color) -> Extents -> a
outline width = rect (inj (pure width :: LRTB Natural))

punct ::
  Inj (CairoElement DrawCtx) a =>
  Text -> a
punct t = textline light1 ubuntuFont t (\_ _ -> Nothing)

ubuntuFont :: Font
ubuntuFont = Font "Ubuntu" 12 FontWeightNormal

-- | Is a precedence border needed?
newtype PrecBorder = PrecBorder Bool

instance Semigroup PrecBorder where
  PrecBorder a <> PrecBorder b = PrecBorder (a || b)

instance Monoid PrecBorder where
  mempty = PrecBorder False

-- | Layouts not enclosed by a precedence border.
newtype PrecUnenclosed = PrecUnenclosed (Set TyId)

instance Semigroup PrecUnenclosed where
  PrecUnenclosed a <> PrecUnenclosed b =
    PrecUnenclosed (Set.union a b)

instance Monoid PrecUnenclosed where
  mempty = PrecUnenclosed Set.empty

addUnenclosed :: TyId -> PrecUnenclosed -> PrecUnenclosed
addUnenclosed tyId (PrecUnenclosed s) =
  PrecUnenclosed (Set.insert tyId s)

guardUnenclosed :: PrecBorder -> PrecUnenclosed -> PrecUnenclosed
guardUnenclosed (PrecBorder True) = const mempty
guardUnenclosed (PrecBorder False) = id

newtype PrecPredicate =
  PrecPredicate { appPrecPredicate :: PrecUnenclosed -> PrecBorder }

precAllow :: Set TyId -> PrecPredicate
precAllow allowed =
  PrecPredicate $ \(PrecUnenclosed unenclosed) ->
    PrecBorder $
      -- Need a border unless all of unenclosed layouts are allowed.
      not (unenclosed `Set.isSubsetOf` allowed)

layoutSel :: PrecBorder -> Path -> Collage Draw -> Collage Draw
layoutSel (PrecBorder precBorder) path =
  collageWithMargin (mkMargin (marginWidth - precBorderWidth)) .
  active outlineWidth path .
  (decorateMargin . DecorationAbove) (outline outlineWidth borderColor) .
  (if precBorder then precedenceBorder precBorderWidth else id) .
  collageWithMargin (mkMargin marginWidth)
  where
    mkMargin a = Margin a a a a
    (marginWidth, precBorderWidth) = (4, 1)
    outlineWidth = 2
    borderColor = mkColor (rgb 94 80 134)
    mkColor color = DrawCtx $ \Paths{pathsSelection} _ ->
      if pathsSelection == path then color else nothing

precedenceBorder :: Natural -> Collage Draw -> Collage Draw
precedenceBorder width a =
  substrate
    (lrtbMargin (collageMargin a))
    (outline width (inj dark2))
    a

lrtbMargin :: Margin -> LRTB Natural
lrtbMargin (Margin l r t b) = lrtb l r t b

active :: Natural -> Path -> Collage Draw -> Collage Draw
active width p =
    (decorateMargin . DecorationAbove) (collageSingleton . activeZone)
  where
    mkColor (Just path) | path == p = Just (rgb 255 127 80)
    mkColor _ = Nothing
    outlineRect = outline width (DrawCtx $ \Paths{pathsCursor} _ -> mkColor pathsCursor)
    activeZone e = DrawEmbed (outlineRect e) p

--------------------------------------------------------------------------------
---- Utils
--------------------------------------------------------------------------------

data EmptyMaybe = EmptyMaybe

nothing :: Inj EmptyMaybe a => a
nothing = inj EmptyMaybe

-- This overlapping instance should make it into inj-base in a better
-- form (no overlapping).

instance {-# OVERLAPPING #-} Inj EmptyMaybe (Maybe a) where
  inj EmptyMaybe = Nothing

maybeA :: Alternative f => Maybe a -> f a
maybeA = maybe empty pure

--------------------------------------------------------------------------------
---- Lenses
--------------------------------------------------------------------------------

makeLenses ''SynStr
makeLenses ''SynRec

--------------------------------------------------------------------------------
---- Editor
--------------------------------------------------------------------------------

data EditorState =
  EditorState
    { _esExpr :: Holey Object,
      _esPointer :: Offset,
      _esHoverBarEnabled :: Bool,
      _esPrecBordersAlways :: Bool,
      _esWritingDirection :: WritingDirection,
      _esUndo :: [Holey Object],
      _esRedo :: [Holey Object]
    }

data NodeCreateFn =
  NodeCreateFn
    { _ncfCheckInputEvent :: InputEvent -> Bool,
      _ncfTyId :: TyId
    }

data LayoutCtx =
  LayoutCtx
    { _lctxPath :: PathBuilder,
      _lctxViewport :: Extents,
      _lctxPrecBordersAlways :: Bool,
      _lctxRecLayouts :: Map TyId ALayoutFn,
      _lctxEnvNameInfo :: EnvNameInfo,
      _lctxWritingDirection :: WritingDirection
    }

data ReactCtx =
  ReactCtx
    { _rctxFindPath :: Offset -> Maybe Path,
      _rctxInputEvent :: InputEvent,
      _rctxNodeFactory :: [NodeCreateFn],
      _rctxDefaultValues :: Map TyId Value,
      _rctxAllowedFieldTypes :: Map FieldId (Set TyId),
      _rctxRecMoveMaps :: Map TyId RecMoveMap,
      _rctxWritingDirection :: WritingDirection
    }

data RecMoveMap =
  RecMoveMap
    { rmmFieldOrder :: [FieldId],
      rmmForward :: Map FieldId FieldId,
      rmmBackward :: Map FieldId FieldId
    }

--------------------------------------------------------------------------------
---- Lenses
--------------------------------------------------------------------------------

makeLenses ''EditorState
makeLenses ''NodeCreateFn
makeLenses ''LayoutCtx
makeLenses ''ReactCtx

--------------------------------------------------------------------------------
---- Utils
--------------------------------------------------------------------------------

keyLetter :: Char -> KeyCode -> Bool
keyLetter c keyCode = keyChar keyCode == Just c

keyCodeLetter :: KeyCode -> Char -> InputEvent -> Bool
keyCodeLetter kc c = \case
  KeyPress [] keyCode -> keyCode == kc || keyLetter c keyCode
  _ -> False

shiftChar :: Char -> InputEvent -> Bool
shiftChar c = \case
  KeyPress [Shift] keyCode -> keyLetter c keyCode
  _ -> False

insertModeEvent :: InputEvent -> Bool
insertModeEvent = \case
  KeyPress [] keyCode -> keyLetter 'i' keyCode
  _ -> False

guardInputEvent ::
  (MonadReader ReactCtx m, MonadPlus m) =>
  (InputEvent -> Bool) ->
  m ()
guardInputEvent = guard <=< views rctxInputEvent

--------------------------------------------------------------------------------
---- Editor - Layout
--------------------------------------------------------------------------------

pprPointer :: Offset -> Text
pprPointer Offset{offsetX,offsetY} =
  Text.pack $ (shows offsetX . (',':) . shows offsetY) ""

layoutEditorState :: LayoutCtx -> EditorState -> Collage Draw
layoutEditorState lctx es =
  (withBars . centered) collage
  where
    (_, collage) = layoutHoleyObject lctx (es ^. esExpr) precPredicate
    precPredicate = PrecPredicate (const (PrecBorder False))
    hoverBar = do
      guard $ es ^. esHoverBarEnabled
      [textWithoutCursor (pprPointer $ es ^. esPointer)]
    selectionBar = do
      let
        path = selectionPathEditorState es
        el = atPath path (es ^. esExpr)
        pathStr = pprPath (lctx ^. lctxEnvNameInfo) path el
      [textWithoutCursor pathStr]
    bars = concat @[] [hoverBar, selectionBar]
    withBars =
      case nonEmpty bars of
        Nothing -> id
        Just bars' -> \c ->
          collageCompose offsetZero c (foldr1 @NonEmpty vertLeft bars')
    centered c =
      let
        padding = centerOf (lctx ^. lctxViewport) c
        backgroundRect = rect nothing (inj dark1)
      in
        substrate padding backgroundRect c

layoutHoleyObject :: LayoutCtx -> Holey Object -> PrecPredicate -> (PrecUnenclosed, Collage Draw)
layoutHoleyObject lctx = \case
  Hole -> \_precPredicate -> layoutHole lctx
  Solid syn -> layoutObject lctx syn

layoutHole :: LayoutCtx -> (PrecUnenclosed, Collage Draw)
layoutHole lctx =
  (,) (mempty @PrecUnenclosed) $
  layoutSel precBorder path $
  punct "_"
  where
    precBorder = PrecBorder (lctx ^. lctxPrecBordersAlways)
    path = buildPath (lctx ^. lctxPath)

layoutObject :: LayoutCtx -> Object -> PrecPredicate -> (PrecUnenclosed, Collage Draw)
layoutObject lctx = \case
  Object tyId (ValueRec syn) -> layoutRec lctx tyId syn
  Object _ (ValueStr syn) -> \_precPredicate -> layoutStr lctx syn

layoutStr :: LayoutCtx -> SynStr -> (PrecUnenclosed, Collage Draw)
layoutStr lctx syn =
  (,) (mempty @PrecUnenclosed) $
  layoutSel precBorder path $
  textWithCursor
    content
    (\Paths{pathsSelection} -> \case
        _ | not (syn ^. synStrEditMode) -> Nothing
        _ | pathsSelection /= path -> Nothing
        CursorInvisible -> Nothing
        CursorVisible -> Just . fromIntegral $ syn ^. synStrPosition)
  where
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways) <>
      PrecBorder (Text.any Char.isSpace content)
    path = buildPath (lctx ^. lctxPath)
    content = syn ^. synStrContent

layoutRec :: LayoutCtx -> TyId -> SynRec -> PrecPredicate -> (PrecUnenclosed, Collage Draw)
layoutRec lctx tyId syn precPredicate =
  (,) (guardUnenclosed precBorder precUnenclosed') $
  layoutSel precBorder path $
  collage
  where
    (precUnenclosed, collage) =
      let
        layoutFields :: RecLayoutFn
        layoutFields =
          case Map.lookup tyId (lctx ^. lctxRecLayouts) of
            Nothing -> fromString (show tyId)
            Just (ALayoutFn fn) -> fn
        drawnFields :: Map FieldId (PrecPredicate -> (PrecUnenclosed, Collage Draw))
        drawnFields =
          Map.mapWithKey
            (\fieldId ->
              let
                pathSegment = PathSegmentRec fieldId
                lctx' = lctx & lctxPath %~ (<> mkPathBuilder pathSegment)
              in
                \obj -> layoutHoleyObject lctx' obj)
            (syn ^. synRecFields)
        wd :: WritingDirection
        wd = lctx ^. lctxWritingDirection
      in
        appRecLayoutFn layoutFields drawnFields tyId wd
    precUnenclosed' = addUnenclosed tyId precUnenclosed
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways) <>
      appPrecPredicate precPredicate precUnenclosed'
    path = buildPath (lctx ^. lctxPath)

--------------------------------------------------------------------------------
---- Editor - Selection
--------------------------------------------------------------------------------

selectionPathEditorState :: EditorState -> Path
selectionPathEditorState es = selectionPathHoleyObject (es ^. esExpr)

selectionPathHoleyObject :: Holey Object -> Path
selectionPathHoleyObject = \case
  Hole -> emptyPath
  Solid (Object _ value) ->
    case value of
      ValueStr _ -> emptyPath
      ValueRec a -> selectionPathRec a

selectionPathRec :: SynRec -> Path
selectionPathRec syn =
  case syn ^. synRecSel of
    RecSelSelf _ -> emptyPath
    RecSelChild fieldId ->
      let
        pathSegment = PathSegmentRec fieldId
        recField = (syn ^. synRecFields) Map.! fieldId
        pathTail = selectionPathHoleyObject recField
      in
        consPath pathSegment pathTail

updatePathEditorState :: Path -> EditorState -> EditorState
updatePathEditorState path = over esExpr (updatePathHoleyObject path)

updatePathHoleyObject :: Path -> Holey Object -> Holey Object
updatePathHoleyObject path = \case
  Hole -> Hole
  Solid (Object tyId value) ->
    Solid (Object tyId (case value of
      ValueStr syn -> ValueStr syn
      ValueRec syn -> ValueRec (updatePathRec path syn)))

updatePathRec :: Path -> SynRec -> SynRec
updatePathRec path syn =
  case unconsPath path of
    Nothing -> syn & synRecSel %~ toRecSelSelf
    Just (PathSegmentSeq _, _) ->
      error "TODO (int-index): updatePathRec PathSegmentSeq"
    Just (PathSegmentRec fieldId, path') ->
      case syn ^. synRecFields . at fieldId of
        Nothing -> syn
        Just a ->
          let
            a' = updatePathHoleyObject path' a
            fields' = Map.insert fieldId a' (syn ^. synRecFields)
          in
            SynRec fields' (RecSelChild fieldId)

toRecSelSelf :: RecSel -> RecSel
toRecSelSelf (RecSelChild fieldId) = RecSelSelf (SelfSelChild fieldId)
toRecSelSelf (RecSelSelf a) = RecSelSelf a

--------------------------------------------------------------------------------
---- Editor - React
--------------------------------------------------------------------------------

type ReactM a = ReaderT ReactCtx (StateT a (MaybeT IO))

runReactM :: ReactM a () -> ReactCtx -> a -> IO (Maybe a)
runReactM m rctx a = runMaybeT (execStateT (runReaderT m rctx) a)

newtype UndoFlag = UndoFlag Bool

pprPath :: EnvNameInfo -> Path -> Maybe TyId -> Text
pprPath nameInfo p0 tip = Text.pack ('/' : goPath p0 "")
  where
    goPath p =
      case unconsPath p of
        Nothing -> goTip
        Just (ps, p') -> goPathSegment ps . ('/':) . goPath p'
    goTip =
      case tip of
        Nothing -> id
        Just tyId ->
          case Map.lookup tyId (envNameInfoTypes nameInfo) of
            Nothing -> shows tyId
            Just tyName -> (tyNameStr tyName++)
    goPathSegment ps =
      case ps of
        PathSegmentRec fieldId ->
          case Map.lookup fieldId (envNameInfoFields nameInfo) of
            Nothing -> shows fieldId
            Just (tyName, fieldName) ->
              (tyNameStr tyName++) . ('.':) . (fieldNameStr fieldName++)
        PathSegmentSeq i -> shows (indexToInt i)

reactEditorState :: ReactCtx -> EditorState -> IO (Maybe EditorState)
reactEditorState = runReactM (asum handlers)
  where
    handlers :: [ReactM EditorState ()]
    handlers =
      [ handlePointerMotion,
        handleButtonPress,
        handleCtrl_h,
        handleCtrl_b,
        handleCtrl_w,
        handleRedirectExpr,
        handleCtrl_z,
        handleCtrl_r ]
    handlePointerMotion = do
      PointerMotion x y <- view rctxInputEvent
      esPointer .= Offset (fromIntegral x) (fromIntegral y)
    handleButtonPress = do
      ButtonPress <- view rctxInputEvent
      Just p <- view rctxFindPath <*> use esPointer
      modify (updatePathEditorState p)
    handleCtrl_h = do
      KeyPress [Control] keyCode <- view rctxInputEvent
      guard $ keyLetter 'h' keyCode
      esHoverBarEnabled %= not
    handleCtrl_b = do
      KeyPress [Control] keyCode <- view rctxInputEvent
      guard $ keyLetter 'b' keyCode
      esPrecBordersAlways %= not
    handleCtrl_w = do
      KeyPress [Control] keyCode <- view rctxInputEvent
      guard $ keyLetter 'w' keyCode
      esWritingDirection %= \case
        WritingDirectionLTR -> WritingDirectionRTL
        WritingDirectionRTL -> WritingDirectionLTR
    handleCtrl_z = do
      KeyPress [Control] keyCode <- view rctxInputEvent
      guard $ keyLetter 'z' keyCode
      (u:us) <- use esUndo
      expr <- use esExpr
      esRedo %= (expr:)
      esUndo .= us
      esExpr .= u
    handleCtrl_r = do
      KeyPress [Control] keyCode <- view rctxInputEvent
      guard $ keyLetter 'r' keyCode
      (r:rs) <- use esRedo
      expr <- use esExpr
      esUndo %= (expr:)
      esRedo .= rs
      esExpr .= r
    handleRedirectExpr = do
      expr <- use esExpr
      let checkTyId = const True -- Allow any construction at the top level.
      UndoFlag undoFlag <- zoom esExpr (reactHoleyObject checkTyId)
      when undoFlag $ do
        esUndo %= (expr:)
        esRedo .= []

reactHoleyObject :: (TyId -> Bool) -> ReactM (Holey Object) UndoFlag
reactHoleyObject checkTyId = asum handlers
  where
    handlers :: [ReactM (Holey Object) UndoFlag]
    handlers =
      [ handleRedirect,
        handleDelete ]
    handleDelete = do
      guardInputEvent $ keyCodeLetter KeyCode.Delete 'x'
      a <- get
      case a of
        Hole -> return (UndoFlag False)
        Solid _ -> do
          put Hole
          return (UndoFlag True)
    handleRedirect =
      ReaderT $ \rctx ->
      StateT $ \case
        Hole ->
          let
            objects =
              [ Object tyId ((rctx ^. rctxDefaultValues) Map.! tyId) |
                ncf <- rctx ^. rctxNodeFactory,
                (ncf ^. ncfCheckInputEvent) (rctx ^. rctxInputEvent),
                let tyId = ncf ^. ncfTyId,
                checkTyId tyId ]
          in
            case objects of
              [] -> MaybeT (return Nothing)
              (a:_) -> return (UndoFlag True, Solid a)
        Solid a -> do
          (undoFlag, a') <- runStateT (runReaderT reactObject rctx) a
          return (undoFlag, Solid a')

mkDefaultValues :: Env -> Map TyId RecMoveMap -> Map TyId Value
mkDefaultValues env recMoveMaps =
  Map.fromList
    [ (mkTyId tyName, mkDefVal tyName ty) |
      (tyName, ty) <- Map.toList (envMap env) ]
  where
    mkDefVal :: TyName -> Ty -> Value
    mkDefVal tyName = \case
      TyStr -> ValueStr (SynStr "" 0 True)
      TySeq _ -> error "TODO (int-index): mkDefVal TySeq"
      TyRec fieldTys ->
        let
          fields = Map.fromList
            [ (mkFieldId tyName fieldName, Hole) |
              (fieldName, _) <- Map.toList fieldTys ]
          recMoveMap = recMoveMaps Map.! mkTyId tyName
          sel =
            case rmmFieldOrder recMoveMap of
              [] -> RecSelSelf SelfSelEmpty
              fieldId:_ -> RecSelChild fieldId
        in
          ValueRec (SynRec fields sel)

reactObject :: ReactM Object UndoFlag
reactObject =
  ReaderT $ \rctx ->
  StateT $ \(Object tyId value) ->
    case value of
      ValueStr syn -> do
        (undoFlag, syn') <- runStateT (runReaderT reactText rctx) syn
        return (undoFlag, Object tyId (ValueStr syn'))
      ValueRec syn -> do
        (undoFlag, syn') <- runStateT (runReaderT (reactRec tyId) rctx) syn
        return (undoFlag, Object tyId (ValueRec syn'))

reactText :: ReactM SynStr UndoFlag
reactText =
  do
    undoFlag <- asum handlers
    modify normalizeSynStr
    return undoFlag
  where
    handlers :: [ReactM SynStr UndoFlag]
    handlers =
      [ handle_i,
        handleEscape,
        handleEnter,
        handleBackspace,
        handleDelete,
        handleArrowLeft,
        handleArrowRight,
        handleLetter ]
    handle_i = do
      False <- use synStrEditMode
      KeyPress [] keyCode <- view rctxInputEvent
      guard (keyLetter 'i' keyCode)
      synStrEditMode .= True
      return (UndoFlag False)
    handleEscape = do
      KeyPress [] KeyCode.Escape <- view rctxInputEvent
      synStrEditMode .= False
      return (UndoFlag False)
    handleEnter = do
      KeyPress [] KeyCode.Enter <- view rctxInputEvent
      synStrEditMode .= False
      return (UndoFlag False)
    handleBackspace = do
      True <- use synStrEditMode
      KeyPress [] KeyCode.Backspace <- view rctxInputEvent
      True <- uses synStrPosition (>0)
      synStrPosition -= 1
      (before, after) <- gets splitSynStr
      synStrContent .= before <> Text.drop 1 after
      return (UndoFlag True)
    handleDelete = do
      True <- use synStrEditMode
      KeyPress [] KeyCode.Delete <- view rctxInputEvent
      (before, after) <- gets splitSynStr
      synStrContent .= before <> Text.drop 1 after
      return (UndoFlag True)
    handleArrowLeft = do
      True <- use synStrEditMode
      KeyPress [] KeyCode.ArrowLeft <- view rctxInputEvent
      synStrPosition -= 1
      return (UndoFlag False)
    handleArrowRight = do
      True <- use synStrEditMode
      KeyPress [] KeyCode.ArrowRight <- view rctxInputEvent
      synStrPosition += 1
      return (UndoFlag False)
    handleLetter = do
      True <- use synStrEditMode
      KeyPress mods keyCode <- view rctxInputEvent
      guard (Control `notElem` mods)
      Just c <- pure (keyChar keyCode)
      modify (insertSynStr (Text.singleton c))
      synStrPosition %= succ
      return (UndoFlag True)

splitSynStr :: SynStr -> (Text, Text)
splitSynStr syn = Text.splitAt (syn ^. synStrPosition) (syn ^. synStrContent)

insertSynStr :: Text -> SynStr -> SynStr
insertSynStr t syn =
  let (before, after) = splitSynStr syn
  in syn & synStrContent .~ before <> t <> after

normalizeSynStr :: SynStr -> SynStr
normalizeSynStr syn = syn & synStrPosition %~ normalizePosition
  where
    normalizePosition :: Int -> Int
    normalizePosition = max 0 . min (views synStrContent Text.length syn)

reactRec :: TyId -> ReactM SynRec UndoFlag
reactRec recTyId = asum handlers
  where
    handlers :: [ReactM SynRec UndoFlag]
    handlers =
      [ handleRedirect,
        handleArrowUp,
        handleArrowDown,
        handleArrowLeft,
        handleArrowRight ]
    handleRedirect :: ReactM SynRec UndoFlag
    handleRedirect = do
      RecSelChild fieldId <- use synRecSel
      allowedFieldTypes <- view rctxAllowedFieldTypes
      let checkTyId tyId = Set.member tyId (allowedFieldTypes Map.! fieldId)
      zoom
        (synRecFields . at fieldId . unsafeSingular _Just)
        (reactHoleyObject checkTyId)
    handleArrowUp :: ReactM SynRec UndoFlag
    handleArrowUp = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
      RecSelChild fieldId <- use synRecSel
      synRecSel .= RecSelSelf (SelfSelChild fieldId)
      return (UndoFlag False)
    handleArrowDown :: ReactM SynRec UndoFlag
    handleArrowDown = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
      RecSelSelf (SelfSelChild fieldId) <- use synRecSel
      synRecSel .= RecSelChild fieldId
      return (UndoFlag False)
    handleArrowLeft :: ReactM SynRec UndoFlag
    handleArrowLeft = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
      RecSelChild fieldId <- use synRecSel
      recMoveMaps <- view rctxRecMoveMaps
      wd <- view rctxWritingDirection
      let
        moveDirection = case wd of
          WritingDirectionLTR -> rmmBackward
          WritingDirectionRTL -> rmmForward
        moveMap = moveDirection (recMoveMaps Map.! recTyId)
      fieldId' <- maybeA (Map.lookup fieldId moveMap)
      synRecSel .= RecSelChild fieldId'
      return (UndoFlag False)
    handleArrowRight :: ReactM SynRec UndoFlag
    handleArrowRight = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
      RecSelChild fieldId <- use synRecSel
      recMoveMaps <- view rctxRecMoveMaps
      wd <- view rctxWritingDirection
      let
        moveDirection = case wd of
          WritingDirectionLTR -> rmmForward
          WritingDirectionRTL -> rmmBackward
        moveMap = moveDirection (recMoveMaps Map.! recTyId)
      fieldId' <- maybeA (Map.lookup fieldId moveMap)
      synRecSel .= RecSelChild fieldId'
      return (UndoFlag False)

mkAllowedFieldTypes :: Env -> Map FieldId (Set TyId)
mkAllowedFieldTypes env =
  Map.fromList
    [ (mkFieldId tyName fieldName, Set.map mkTyId tys) |
      (tyName, TyRec fields) <- Map.toList (envMap env),
      (fieldName, TyUnion tys) <- Map.toList fields ]

mkRecMoveMaps :: Map TyId ALayoutFn -> Map TyId RecMoveMap
mkRecMoveMaps recLayouts =
  Map.mapWithKey mkRecMoveMap recLayouts

mkRecMoveMap :: TyId -> ALayoutFn -> RecMoveMap
mkRecMoveMap tyId recLayoutFn =
  RecMoveMap
    { rmmFieldOrder = sortedFieldIds,
      rmmForward = seqToMoveMap sortedFieldIds,
      rmmBackward = seqToMoveMap (List.reverse sortedFieldIds) }
  where
    seqToMoveMap xs =
      case xs of
        [] -> Map.empty
        _:xs' -> Map.fromList (List.zip xs xs')
    sortedFieldIds =
      sortByVisualOrder tyId recLayoutFn

newtype VisualFieldList = VisualFieldList (TyId -> [FieldId])

instance IsString VisualFieldList where
  fromString _ = VisualFieldList (const [])

instance Semigroup VisualFieldList where
  VisualFieldList a <> VisualFieldList b =
    VisualFieldList (a <> b)

instance Layout VisualFieldList where
  VisualFieldList a `vsep` VisualFieldList b =
    VisualFieldList (a <> b)
  field fieldName _ =
    VisualFieldList $
      \tyId -> [mkFieldId' tyId fieldName]

sortByVisualOrder :: TyId -> ALayoutFn -> [FieldId]
sortByVisualOrder tyId recLayoutFn = mkSortedFields tyId
  where
    mkSortedFields :: TyId -> [FieldId]
    ALayoutFn (VisualFieldList mkSortedFields) = recLayoutFn

--------------------------------------------------------------------------------
---- Plugin
--------------------------------------------------------------------------------

-- | A plugin as specified by the user.
data Plugin =
  Plugin
    { _pluginTyEnv :: Env,
      _pluginRecLayouts :: Map TyId ALayoutFn,
      _pluginNodeFactory :: [NodeCreateFn]
    }

-- | A plugin as consumed by the editor, with additional information
-- derived from the user specification.
data PluginInfo =
  PluginInfo
    { _pluginInfoTyEnv :: Env,
      _pluginInfoRecLayouts :: Map TyId ALayoutFn,
      _pluginInfoNodeFactory :: [NodeCreateFn],
      _pluginInfoEnvNameInfo :: EnvNameInfo,
      _pluginInfoRecMoveMaps :: Map TyId RecMoveMap,
      _pluginInfoDefaultValues :: Map TyId Value,
      _pluginInfoAllowedFieldTypes :: Map FieldId (Set TyId)
    }

makeLenses ''Plugin
makeLenses ''PluginInfo

mkPluginInfo :: Plugin -> PluginInfo
mkPluginInfo plugin =
  PluginInfo
    { _pluginInfoTyEnv = tyEnv,
      _pluginInfoRecLayouts = recLayouts,
      _pluginInfoNodeFactory = nodeFactory,
      _pluginInfoEnvNameInfo = envNameInfo,
      _pluginInfoRecMoveMaps = recMoveMaps,
      _pluginInfoDefaultValues = defaultValues,
      _pluginInfoAllowedFieldTypes = allowedFieldTypes
    }
  where
    tyEnv = plugin ^. pluginTyEnv
    recLayouts = plugin ^. pluginRecLayouts
    nodeFactory = plugin ^. pluginNodeFactory
    envNameInfo = buildEnvNameInfo tyEnv
    recMoveMaps = mkRecMoveMaps recLayouts
    defaultValues = mkDefaultValues tyEnv recMoveMaps
    allowedFieldTypes = mkAllowedFieldTypes tyEnv
