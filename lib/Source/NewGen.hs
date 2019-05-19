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
  Selection(..),
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
  ncfTyName,

  LayoutCtx(..),
  lctxPath,
  lctxViewport,
  lctxPrecBordersAlways,
  lctxRecLayouts,
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
  selectionOfEditorState,
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
  pluginInfoRecMoveMaps,
  pluginInfoDefaultValues,
  pluginInfoAllowedFieldTypes,
  mkPluginInfo,

  -- * Utils
  inj,
  nothing,
  maybeA

  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
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

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
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

mkTyUnion :: [TyName] -> TyUnion
mkTyUnion = TyUnion . HashSet.fromList

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data Holey a =
  Hole |
  Solid a

data Object =
  Object TyName Value

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
  SelfSelChild FieldName

data RecSel =
  RecSelSelf SelfSel |
  RecSelChild FieldName

data SynRec =
  SynRec
    { _synRecFields :: HashMap FieldName (Holey Object),
      _synRecSel :: RecSel
    }

--------------------------------------------------------------------------------
---- Drawing
--------------------------------------------------------------------------------

data CursorBlink = CursorVisible | CursorInvisible

blink :: CursorBlink -> CursorBlink
blink = \case
  CursorVisible -> CursorInvisible
  CursorInvisible -> CursorVisible

data Selection =
  Selection
    { selectionPath :: Path,
      selectionTyName :: Maybe TyName,
      selectionStrPos :: Maybe Int }

data Paths =
  Paths
    { pathsCursor :: Maybe Path,
      pathsSelection :: Selection }

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
      HashMap FieldName (PrecPredicate -> (PrecUnenclosed, Collage Draw)) ->
      WritingDirection ->
      (PrecUnenclosed, Collage Draw)
  }

instance IsString RecLayoutFn where
  fromString s =
    RecLayoutFn $ \_ _ ->
      (mempty, punct (fromString s))

instance Semigroup RecLayoutFn where
  RecLayoutFn a <> RecLayoutFn b =
    RecLayoutFn $ \m wd ->
      let
        (aUnenclosed, a') = a m wd
        (bUnenclosed, b') = b m wd
        f = case wd of
          WritingDirectionLTR -> horizBaseline
          WritingDirectionRTL -> flip horizBaseline
      in
        (,) (aUnenclosed <> bUnenclosed) $
        f a' b'

instance Layout RecLayoutFn where
  RecLayoutFn a `vsep` RecLayoutFn b =
    RecLayoutFn $ \m wd ->
      let
        (aUnenclosed, a') = a m wd
        (bUnenclosed, b') = b m wd
        f = case wd of
          WritingDirectionLTR -> vertLeft
          WritingDirectionRTL -> vertRight
        maxWidth = (max `on` widthOf) a' b'
      in
        (,) (aUnenclosed <> bUnenclosed) $
        a' `f` line light1 maxWidth `f` b'
  field fieldName precPredicate =
    RecLayoutFn $ \m _ ->
      (m HashMap.! fieldName) precPredicate

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
newtype PrecUnenclosed = PrecUnenclosed (HashSet TyName)

instance Semigroup PrecUnenclosed where
  PrecUnenclosed a <> PrecUnenclosed b =
    PrecUnenclosed (HashSet.union a b)

instance Monoid PrecUnenclosed where
  mempty = PrecUnenclosed HashSet.empty

addUnenclosed :: TyName -> PrecUnenclosed -> PrecUnenclosed
addUnenclosed tyName (PrecUnenclosed s) =
  PrecUnenclosed (HashSet.insert tyName s)

guardUnenclosed :: PrecBorder -> PrecUnenclosed -> PrecUnenclosed
guardUnenclosed (PrecBorder True) = const mempty
guardUnenclosed (PrecBorder False) = id

newtype PrecPredicate =
  PrecPredicate { appPrecPredicate :: PrecUnenclosed -> PrecBorder }

precAllow :: HashSet TyName -> PrecPredicate
precAllow allowed =
  PrecPredicate $ \(PrecUnenclosed unenclosed) ->
    PrecBorder $
      -- Need a border unless all of unenclosed layouts are allowed.
      not (unenclosed `hashSet_isSubsetOf` allowed)

hashSet_isSubsetOf :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
hashSet_isSubsetOf sub sup =
  all (\k -> HashSet.member k sup) sub

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
      if selectionPath pathsSelection == path then color else nothing

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
      _ncfTyName :: TyName
    }

data LayoutCtx =
  LayoutCtx
    { _lctxPath :: PathBuilder,
      _lctxViewport :: Extents,
      _lctxPrecBordersAlways :: Bool,
      _lctxRecLayouts :: HashMap TyName ALayoutFn,
      _lctxWritingDirection :: WritingDirection
    }

data ReactCtx =
  ReactCtx
    { _rctxFindPath :: Offset -> Maybe Path,
      _rctxInputEvent :: InputEvent,
      _rctxNodeFactory :: [NodeCreateFn],
      _rctxDefaultValues :: HashMap TyName Value,
      _rctxAllowedFieldTypes :: HashMap (TyName, FieldName) (HashSet TyName),
      _rctxRecMoveMaps :: HashMap TyName RecMoveMap,
      _rctxWritingDirection :: WritingDirection
    }

data RecMoveMap =
  RecMoveMap
    { rmmFieldOrder :: [FieldName],
      rmmForward :: HashMap FieldName FieldName,
      rmmBackward :: HashMap FieldName FieldName
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
      let sel = selectionOfEditorState es
      [textWithoutCursor (pprSelection sel)]
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

pprSelection :: Selection -> Text
pprSelection selection = Text.pack ('/' : goPath selectionPath "")
  where
    Selection{selectionPath, selectionTyName, selectionStrPos} = selection
    goPath p =
      case unconsPath p of
        Nothing -> goTip selectionTyName
        Just (ps, p') -> goPathSegment ps . ('/':) . goPath p'
    goTip Nothing = id
    goTip (Just tyName) = (tyNameStr tyName++) . goStrPos selectionStrPos
    goPathSegment ps =
      case ps of
        PathSegmentRec tyName fieldName ->
          (tyNameStr tyName++) . ('.':) . (fieldNameStr fieldName++)
        PathSegmentSeq tyName i ->
          (tyNameStr tyName++) . ('.':) . shows (indexToInt i)
    goStrPos Nothing = id
    goStrPos (Just i) = ('[':) . shows i . (']':)

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

layoutObject ::
  LayoutCtx ->
  Object ->
  PrecPredicate ->
  (PrecUnenclosed, Collage Draw)
layoutObject lctx = \case
  Object tyName (ValueRec syn) -> layoutRec lctx tyName (syn ^. synRecFields)
  Object _ (ValueStr syn) -> \_precPredicate -> layoutStr lctx (syn ^. synStrContent)

layoutStr :: LayoutCtx -> Text -> (PrecUnenclosed, Collage Draw)
layoutStr lctx str =
  (,) (mempty @PrecUnenclosed) $
  layoutSel precBorder path $
  textWithCursor str
    (\Paths{pathsSelection} ->
     \case
       CursorVisible
         | selectionPath pathsSelection == path,
           Just pos <- selectionStrPos pathsSelection
         ->
           Just (fromIntegral pos)
       _ -> Nothing)
  where
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways) <>
      PrecBorder (Text.any Char.isSpace str)
    path = buildPath (lctx ^. lctxPath)

layoutRec ::
  LayoutCtx ->
  TyName ->
  HashMap FieldName (Holey Object) ->
  PrecPredicate ->
  (PrecUnenclosed, Collage Draw)
layoutRec lctx tyName fields precPredicate =
  (,) (guardUnenclosed precBorder precUnenclosed') $
  layoutSel precBorder path $
  collage
  where
    (precUnenclosed, collage) =
      let
        layoutFields :: RecLayoutFn
        layoutFields =
          case HashMap.lookup tyName (lctx ^. lctxRecLayouts) of
            Nothing -> fromString (show tyName)
            Just (ALayoutFn fn) -> fn
        drawnFields :: HashMap FieldName (PrecPredicate -> (PrecUnenclosed, Collage Draw))
        drawnFields =
          HashMap.mapWithKey
            (\fieldName ->
              let
                pathSegment = PathSegmentRec tyName fieldName
                lctx' = lctx & lctxPath %~ (<> mkPathBuilder pathSegment)
              in
                \obj -> layoutHoleyObject lctx' obj)
            fields
        wd :: WritingDirection
        wd = lctx ^. lctxWritingDirection
      in
        appRecLayoutFn layoutFields drawnFields wd
    precUnenclosed' = addUnenclosed tyName precUnenclosed
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways) <>
      appPrecPredicate precPredicate precUnenclosed'
    path = buildPath (lctx ^. lctxPath)

--------------------------------------------------------------------------------
---- Editor - Selection
--------------------------------------------------------------------------------

selectionOfEditorState :: EditorState -> Selection
selectionOfEditorState es = selectionOfHoleyObject (es ^. esExpr)

selectionOfHoleyObject :: Holey Object -> Selection
selectionOfHoleyObject = \case
  Hole -> Selection emptyPath Nothing Nothing
  Solid (Object tyName value) ->
    case value of
      ValueStr a -> selectionOfStr tyName a
      ValueRec a -> selectionOfRec tyName a

selectionOfRec :: TyName -> SynRec -> Selection
selectionOfRec tyName syn =
  case syn ^. synRecSel of
    RecSelSelf _ -> Selection emptyPath (Just tyName) Nothing
    RecSelChild fieldName ->
      let
        pathSegment = PathSegmentRec tyName fieldName
        recField = (syn ^. synRecFields) HashMap.! fieldName
        Selection pathTail tyName' strPos =
          selectionOfHoleyObject recField
      in
        Selection (consPath pathSegment pathTail) tyName' strPos

selectionOfStr :: TyName -> SynStr -> Selection
selectionOfStr tyName syn =
    Selection emptyPath (Just tyName) strPos
  where
    strPos
      | syn ^. synStrEditMode = Just (syn ^. synStrPosition)
      | otherwise = Nothing

updatePathEditorState :: Path -> EditorState -> EditorState
updatePathEditorState path = over esExpr (updatePathHoleyObject path)

updatePathHoleyObject :: Path -> Holey Object -> Holey Object
updatePathHoleyObject path = \case
  Hole -> Hole
  Solid (Object tyName value) ->
    Solid (Object tyName (case value of
      ValueStr syn -> ValueStr syn
      ValueRec syn -> ValueRec (updatePathRec tyName path syn)))

updatePathRec :: TyName -> Path -> SynRec -> SynRec
updatePathRec tyName path syn =
  case unconsPath path of
    Nothing -> syn & synRecSel %~ toRecSelSelf
    Just (PathSegmentSeq _ _, _) ->
      error "TODO (int-index): updatePathRec PathSegmentSeq"
    Just (PathSegmentRec tyName' fieldName, path') ->
      if tyName' /= tyName then syn else
      case syn ^. synRecFields . at fieldName of
        Nothing -> syn
        Just a ->
          let
            a' = updatePathHoleyObject path' a
            fields' = HashMap.insert fieldName a' (syn ^. synRecFields)
          in
            SynRec fields' (RecSelChild fieldName)

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
      let checkTyName = const True -- Allow any construction at the top level.
      UndoFlag undoFlag <- zoom esExpr (reactHoleyObject checkTyName)
      when undoFlag $ do
        esUndo %= (expr:)
        esRedo .= []

reactHoleyObject :: (TyName -> Bool) -> ReactM (Holey Object) UndoFlag
reactHoleyObject checkTyName = asum handlers
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
              [ Object tyName ((rctx ^. rctxDefaultValues) HashMap.! tyName) |
                ncf <- rctx ^. rctxNodeFactory,
                (ncf ^. ncfCheckInputEvent) (rctx ^. rctxInputEvent),
                let tyName = ncf ^. ncfTyName,
                checkTyName tyName ]
          in
            case objects of
              [] -> MaybeT (return Nothing)
              (a:_) -> return (UndoFlag True, Solid a)
        Solid a -> do
          (undoFlag, a') <- runStateT (runReaderT reactObject rctx) a
          return (undoFlag, Solid a')

mkDefaultValues :: Env -> HashMap TyName RecMoveMap -> HashMap TyName Value
mkDefaultValues env recMoveMaps =
  HashMap.mapWithKey mkDefVal (envMap env)
  where
    mkDefVal :: TyName -> Ty -> Value
    mkDefVal tyName = \case
      TyStr -> ValueStr (SynStr "" 0 True)
      TySeq _ -> error "TODO (int-index): mkDefVal TySeq"
      TyRec fieldTys ->
        let
          fields = HashMap.map (const Hole) fieldTys
          recMoveMap = recMoveMaps HashMap.! tyName
          sel =
            case rmmFieldOrder recMoveMap of
              [] -> RecSelSelf SelfSelEmpty
              fieldName:_ -> RecSelChild fieldName
        in
          ValueRec (SynRec fields sel)

reactObject :: ReactM Object UndoFlag
reactObject =
  ReaderT $ \rctx ->
  StateT $ \(Object tyName value) ->
    case value of
      ValueStr syn -> do
        (undoFlag, syn') <- runStateT (runReaderT reactText rctx) syn
        return (undoFlag, Object tyName (ValueStr syn'))
      ValueRec syn -> do
        (undoFlag, syn') <- runStateT (runReaderT (reactRec tyName) rctx) syn
        return (undoFlag, Object tyName (ValueRec syn'))

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

reactRec :: TyName -> ReactM SynRec UndoFlag
reactRec recTyName = asum handlers
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
      RecSelChild fieldName <- use synRecSel
      allowedFieldTypes <- view rctxAllowedFieldTypes
      let checkTyName tyName = HashSet.member tyName (allowedFieldTypes HashMap.! (recTyName, fieldName))
      zoom
        (synRecFields . at fieldName . unsafeSingular _Just)
        (reactHoleyObject checkTyName)
    handleArrowUp :: ReactM SynRec UndoFlag
    handleArrowUp = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
      RecSelChild fieldName <- use synRecSel
      synRecSel .= RecSelSelf (SelfSelChild fieldName)
      return (UndoFlag False)
    handleArrowDown :: ReactM SynRec UndoFlag
    handleArrowDown = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
      RecSelSelf (SelfSelChild fieldName) <- use synRecSel
      synRecSel .= RecSelChild fieldName
      return (UndoFlag False)
    handleArrowLeft :: ReactM SynRec UndoFlag
    handleArrowLeft = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
      RecSelChild fieldName <- use synRecSel
      recMoveMaps <- view rctxRecMoveMaps
      wd <- view rctxWritingDirection
      let
        moveDirection = case wd of
          WritingDirectionLTR -> rmmBackward
          WritingDirectionRTL -> rmmForward
        moveMap = moveDirection (recMoveMaps HashMap.! recTyName)
      fieldName' <- maybeA (HashMap.lookup fieldName moveMap)
      synRecSel .= RecSelChild fieldName'
      return (UndoFlag False)
    handleArrowRight :: ReactM SynRec UndoFlag
    handleArrowRight = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
      RecSelChild fieldName <- use synRecSel
      recMoveMaps <- view rctxRecMoveMaps
      wd <- view rctxWritingDirection
      let
        moveDirection = case wd of
          WritingDirectionLTR -> rmmForward
          WritingDirectionRTL -> rmmBackward
        moveMap = moveDirection (recMoveMaps HashMap.! recTyName)
      fieldName' <- maybeA (HashMap.lookup fieldName moveMap)
      synRecSel .= RecSelChild fieldName'
      return (UndoFlag False)

mkAllowedFieldTypes :: Env -> HashMap (TyName, FieldName) (HashSet TyName)
mkAllowedFieldTypes env =
  HashMap.fromList
    [ ((tyName, fieldName), tys) |
      (tyName, TyRec fields) <- HashMap.toList (envMap env),
      (fieldName, TyUnion tys) <- HashMap.toList fields ]

mkRecMoveMaps :: HashMap TyName ALayoutFn -> HashMap TyName RecMoveMap
mkRecMoveMaps = HashMap.map mkRecMoveMap

mkRecMoveMap :: ALayoutFn -> RecMoveMap
mkRecMoveMap recLayoutFn =
  RecMoveMap
    { rmmFieldOrder = sortedFieldNames,
      rmmForward = seqToMoveMap sortedFieldNames,
      rmmBackward = seqToMoveMap (List.reverse sortedFieldNames) }
  where
    seqToMoveMap xs =
      case xs of
        [] -> HashMap.empty
        _:xs' -> HashMap.fromList (List.zip xs xs')
    sortedFieldNames =
      sortByVisualOrder recLayoutFn

newtype VisualFieldList = VisualFieldList [FieldName]

instance IsString VisualFieldList where
  fromString _ = VisualFieldList []

instance Semigroup VisualFieldList where
  VisualFieldList a <> VisualFieldList b =
    VisualFieldList (a <> b)

instance Layout VisualFieldList where
  VisualFieldList a `vsep` VisualFieldList b =
    VisualFieldList (a <> b)
  field fieldName _ = VisualFieldList [fieldName]

sortByVisualOrder :: ALayoutFn -> [FieldName]
sortByVisualOrder recLayoutFn = sortedFields
  where
    sortedFields :: [FieldName]
    ALayoutFn (VisualFieldList sortedFields) = recLayoutFn

--------------------------------------------------------------------------------
---- Plugin
--------------------------------------------------------------------------------

-- | A plugin as specified by the user.
data Plugin =
  Plugin
    { _pluginTyEnv :: Env,
      _pluginRecLayouts :: HashMap TyName ALayoutFn,
      _pluginNodeFactory :: [NodeCreateFn]
    }

-- | A plugin as consumed by the editor, with additional information
-- derived from the user specification.
data PluginInfo =
  PluginInfo
    { _pluginInfoTyEnv :: Env,
      _pluginInfoRecLayouts :: HashMap TyName ALayoutFn,
      _pluginInfoNodeFactory :: [NodeCreateFn],
      _pluginInfoRecMoveMaps :: HashMap TyName RecMoveMap,
      _pluginInfoDefaultValues :: HashMap TyName Value,
      _pluginInfoAllowedFieldTypes :: HashMap (TyName, FieldName) (HashSet TyName)
    }

makeLenses ''Plugin
makeLenses ''PluginInfo

mkPluginInfo :: Plugin -> PluginInfo
mkPluginInfo plugin =
  PluginInfo
    { _pluginInfoTyEnv = tyEnv,
      _pluginInfoRecLayouts = recLayouts,
      _pluginInfoNodeFactory = nodeFactory,
      _pluginInfoRecMoveMaps = recMoveMaps,
      _pluginInfoDefaultValues = defaultValues,
      _pluginInfoAllowedFieldTypes = allowedFieldTypes
    }
  where
    tyEnv = plugin ^. pluginTyEnv
    recLayouts = plugin ^. pluginRecLayouts
    nodeFactory = plugin ^. pluginNodeFactory
    recMoveMaps = mkRecMoveMaps recLayouts
    defaultValues = mkDefaultValues tyEnv recMoveMaps
    allowedFieldTypes = mkAllowedFieldTypes tyEnv
