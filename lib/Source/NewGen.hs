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
  Node(..),
  Object(..),
  Value(..),

  NodeSel(..),
  RecSel(..),

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
  ReactResult(..),
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
  rctxTyEnv,
  rctxFindPath,
  rctxNodeFactory,
  rctxDefaultNodes,
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
  pluginInfoDefaultNodes,
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
import Control.Monad.Writer
import Control.Lens as Lens hiding (elements)
import Data.Function (on)
import Data.String
import Data.Maybe
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

import Sdam.Core

mkTyUnion :: [TyName] -> TyUnion
mkTyUnion = TyUnion . HashSet.fromList

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data Node = Hole | Node NodeSel (Object Node)

data NodeSel =
  NodeRecSel RecSel |
  NodeStrSel Int Bool   -- True = edit mode

data RecSel =
  -- for records without children
  RecSel0 |
  -- for records with children
  RecSel FieldName Bool   -- True = child selected

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
---- Editor
--------------------------------------------------------------------------------

data EditorState =
  EditorState
    { _esExpr :: Node,
      _esPointer :: Offset,
      _esHoverBarEnabled :: Bool,
      _esPrecBordersAlways :: Bool,
      _esWritingDirection :: WritingDirection,
      _esUndo :: [Node],
      _esRedo :: [Node]
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
    { _rctxTyEnv :: Env,
      _rctxFindPath :: Offset -> Maybe Path,
      _rctxNodeFactory :: [NodeCreateFn],
      _rctxDefaultNodes :: HashMap TyName Node,
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
    (_, collage) = layoutNode lctx (es ^. esExpr) precPredicate
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

layoutNode :: LayoutCtx -> Node -> PrecPredicate -> (PrecUnenclosed, Collage Draw)
layoutNode lctx = \case
  Hole -> \_precPredicate -> layoutHole lctx
  Node _ object -> layoutObject lctx object

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
  Object Node ->
  PrecPredicate ->
  (PrecUnenclosed, Collage Draw)
layoutObject lctx = \case
  Object tyName (ValueRec fields) -> layoutRec lctx tyName fields
  Object _ (ValueSeq _) -> error "TODO (int-index): layoutObject ValueSeq"
  Object _ (ValueStr str) -> \_precPredicate -> layoutStr lctx str

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
  HashMap FieldName Node ->
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
                \obj -> layoutNode lctx' obj)
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
selectionOfEditorState es = selectionOfNode (es ^. esExpr)

selectionOfNode :: Node -> Selection
selectionOfNode = \case
  Hole -> Selection emptyPath Nothing Nothing
  Node nodeSel (Object tyName (ValueStr _)) ->
    let NodeStrSel pos em = nodeSel
    in Selection emptyPath (Just tyName) (if em then Just pos else Nothing)
  Node _ (Object _ (ValueSeq _)) ->
    error "TODO (int-index): selectionOfNode ValueSeq"
  Node nodeSel (Object tyName (ValueRec fields)) ->
    let NodeRecSel recSel = nodeSel in
    case recSel of
      RecSel0 ->
        Selection emptyPath (Just tyName) Nothing
      RecSel _ False ->
        Selection emptyPath (Just tyName) Nothing
      RecSel fieldName True ->
        let
          pathSegment = PathSegmentRec tyName fieldName
          recField = fields HashMap.! fieldName
          Selection pathTail tyName' strPos =
            selectionOfNode recField
        in
          Selection (consPath pathSegment pathTail) tyName' strPos

updatePathNode :: Path -> Node -> Node
updatePathNode path node = case node of
  Hole -> node
  Node _ (Object _ (ValueStr _)) -> node
  Node _ (Object _ (ValueSeq _)) ->
    error "TODO (int-index): updatePathNode ValueSeq"
  Node nodeSel (Object tyName (ValueRec fields)) ->
    let NodeRecSel recSel = nodeSel in
    case unconsPath path of
      Nothing ->
        let recSel' = toRecSelSelf recSel
        in Node (NodeRecSel recSel') (Object tyName (ValueRec fields))
      Just (PathSegmentSeq _ _, _) ->
        error "TODO (int-index): updatePathRec PathSegmentSeq"
      Just (PathSegmentRec tyName' fieldName, path') ->
        if tyName' /= tyName then node else
        case fields ^. at fieldName of
          Nothing -> node
          Just a ->
            let
              a' = updatePathNode path' a
              fields' = HashMap.insert fieldName a' fields
              recSel' = RecSel fieldName True
            in
              Node (NodeRecSel recSel') (Object tyName (ValueRec fields'))

toRecSelSelf :: RecSel -> RecSel
toRecSelSelf RecSel0 = RecSel0
toRecSelSelf (RecSel fieldName _) = RecSel fieldName False

toRecSelChild :: RecSel -> Maybe RecSel
toRecSelChild RecSel0 = Nothing
toRecSelChild (RecSel fieldName _) = Just (RecSel fieldName True)

--------------------------------------------------------------------------------
---- Editor - React
--------------------------------------------------------------------------------

setUndoFlag :: MonadWriter UndoFlag m => m ()
setUndoFlag = tell (UndoFlag True)

newtype UndoFlag = UndoFlag Bool

instance Semigroup UndoFlag where
  UndoFlag u1 <> UndoFlag u2 = UndoFlag (u1 || u2)

instance Monoid UndoFlag where
  mempty = UndoFlag False

data ReactResult a = UnknownEvent | ReactOk a

reactEditorState :: InputEvent -> ReactCtx -> EditorState -> ReactResult EditorState

reactEditorState (PointerMotion x y) _ es = ReactOk $
  es & esPointer .~ Offset (fromIntegral x) (fromIntegral y)

reactEditorState ButtonPress rctx es
  | Just p <- (rctx ^. rctxFindPath) (es ^. esPointer)
  = ReactOk $ es & esExpr %~ updatePathNode p

reactEditorState (KeyPress [Control] keyCode) _ es
  | keyLetter 'h' keyCode
  = ReactOk $ es & esHoverBarEnabled %~ not
  | keyLetter 'b' keyCode
  = ReactOk $ es & esPrecBordersAlways %~ not
  | keyLetter 'w' keyCode
  = ReactOk $
    es & esWritingDirection %~ \case
      WritingDirectionLTR -> WritingDirectionRTL
      WritingDirectionRTL -> WritingDirectionLTR
  | keyLetter 'z' keyCode,
    (u:us) <- es ^. esUndo,
    let expr = es ^. esExpr
  = ReactOk $
    es & esExpr .~ u
       & esUndo .~ us
       & esRedo %~ (expr:)
  | keyLetter 'r' keyCode,
    (r:rs) <- es ^. esRedo,
    let expr = es ^. esExpr
  = ReactOk $
    es & esExpr .~ r
       & esRedo .~ rs
       & esUndo %~ (expr:)

reactEditorState inputEvent rctx es
  | Just act <- getAction env nodeFactory wd sel inputEvent,
    Just (UndoFlag undoFlag, expr') <- applyAction act rctx expr
  = ReactOk $
    if undoFlag then
      es & esExpr .~ expr'
         & esUndo %~ (expr:)
         & esRedo .~ []
    else
      es & esExpr .~ expr'
  where
    env = rctx ^. rctxTyEnv
    nodeFactory = rctx ^. rctxNodeFactory
    wd = rctx ^. rctxWritingDirection
    sel = selectionOfNode expr
    expr = es ^. esExpr

reactEditorState _ _ _ = UnknownEvent

newNodeTyName :: [NodeCreateFn] -> InputEvent -> Maybe TyName
newNodeTyName nodeFactory inputEvent =
  listToMaybe $ do
    ncf <- nodeFactory
    guard $ (ncf ^. ncfCheckInputEvent) inputEvent
    [ncf ^. ncfTyName]

data Action =
  ActionDeleteNode Path |
  ActionCreateNode Path TyName |
  ActionEnterEditMode Path |
  ActionExitEditMode Path |
  ActionDeleteCharBackward Path |
  ActionDeleteCharForward Path |
  ActionMoveStrCursorBackward Path |
  ActionMoveStrCursorForward Path |
  ActionInsertLetter Path Char |
  ActionSelectParent Path |
  ActionSelectChild Path |
  ActionSelectSiblingBackward Path |
  ActionSelectSiblingForward Path

getAction ::
  Env ->
  [NodeCreateFn] ->
  WritingDirection ->
  Selection ->
  InputEvent ->
  Maybe Action
getAction
    Env{envMap}
    nodeFactory
    wd
    Selection{selectionPath, selectionTyName, selectionStrPos}
    inputEvent

  -- Enter edit mode.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    Nothing <- selectionStrPos,
    KeyPress [] keyCode <- inputEvent,
    keyLetter 'i' keyCode
  = Just $ ActionEnterEditMode selectionPath

  -- Exit edit mode.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    KeyPress [] KeyCode.Escape <- inputEvent
  = Just $ ActionExitEditMode selectionPath

  -- Delete character backward.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.Backspace <- inputEvent
  = Just $ ActionDeleteCharBackward selectionPath

  -- Delete character forward.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.Delete <- inputEvent
  = Just $ ActionDeleteCharForward selectionPath

  -- Move string cursor backward.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.ArrowLeft <- inputEvent
  = Just $ ActionMoveStrCursorBackward selectionPath

  -- Move string cursor forward.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.ArrowRight <- inputEvent
  = Just $ ActionMoveStrCursorForward selectionPath

  -- Insert letter.
  | Just tyName <- selectionTyName,
    Just TyStr <- HashMap.lookup tyName envMap,
    Just _ <- selectionStrPos,
    KeyPress mods keyCode <- inputEvent,
    Control `notElem` mods,
    Just c <- keyChar keyCode
  = Just $ ActionInsertLetter selectionPath c

  -- Delete node.
  | keyCodeLetter KeyCode.Delete 'x' inputEvent
  = Just $ ActionDeleteNode selectionPath

  -- Create node.
  | Nothing <- selectionTyName,  -- it's a hole
    Just tyName <- newNodeTyName nodeFactory inputEvent
  = Just $ ActionCreateNode selectionPath tyName

  -- Select parent node.
  | keyCodeLetter KeyCode.ArrowUp 'k' inputEvent
  = Just $ ActionSelectParent selectionPath

  -- Select child node.
  | keyCodeLetter KeyCode.ArrowDown 'j' inputEvent
  = Just $ ActionSelectChild selectionPath

  -- Select sibling node left.
  | keyCodeLetter KeyCode.ArrowLeft 'h' inputEvent
  = Just $ case wd of
      WritingDirectionLTR -> ActionSelectSiblingBackward selectionPath
      WritingDirectionRTL -> ActionSelectSiblingForward selectionPath

  -- Select sibling node right.
  | keyCodeLetter KeyCode.ArrowRight 'l' inputEvent
  = Just $ case wd of
      WritingDirectionLTR -> ActionSelectSiblingForward selectionPath
      WritingDirectionRTL -> ActionSelectSiblingBackward selectionPath

  | otherwise
  = Nothing

type ReactM = WriterT UndoFlag (ReaderT ReactCtx (StateT Node Maybe)) ()

applyAction :: Action -> ReactCtx -> Node -> Maybe (UndoFlag, Node)
applyAction act rctx node =
  flip runStateT node $
  flip runReaderT rctx $
  execWriterT $
  applyActionM act

applyActionM :: Action -> ReactM

applyActionM (ActionDeleteNode path) =
  zoom (atPath path) $ do
    Node{} <- get
    put Hole
    setUndoFlag

applyActionM (ActionCreateNode path tyName) =
  zoom (atPath path) $ do
    Hole <- get
    allowedFieldTypes <- view rctxAllowedFieldTypes
    guard (validChild allowedFieldTypes)
    defaultNodes <- view rctxDefaultNodes
    let node = defaultNodes HashMap.! tyName
    put node
    setUndoFlag
  where
    validChild allowedFieldTypes =
      case pathTip path of
        Nothing -> True
        Just (PathSegmentSeq _ _) ->
          error "TODO (int-index): validChild PathSegmentSeq"
        Just (PathSegmentRec recTyName fieldName) ->
          HashSet.member tyName (allowedFieldTypes HashMap.! (recTyName, fieldName))

applyActionM (ActionEnterEditMode path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos _ = nodeSel
    put $ Node (NodeStrSel pos True) (Object tyName (ValueStr str))

applyActionM (ActionExitEditMode path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos _ = nodeSel
    put $ Node (NodeStrSel pos False) (Object tyName (ValueStr str))

applyActionM (ActionDeleteCharBackward path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos em = nodeSel
    guard (pos > 0)
    let pos' = pos - 1
    let (before, after) = Text.splitAt pos' str
        str' = before <> Text.drop 1 after
    put $ Node (NodeStrSel pos' em) (Object tyName (ValueStr str'))
    setUndoFlag

applyActionM (ActionDeleteCharForward path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos _ = nodeSel
    guard (pos < Text.length str)
    let (before, after) = Text.splitAt pos str
        str' = before <> Text.drop 1 after
    put $ Node nodeSel (Object tyName (ValueStr str'))
    setUndoFlag

applyActionM (ActionMoveStrCursorBackward path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos em = nodeSel
    guard (pos > 0)
    let pos' = pos - 1
    put $ Node (NodeStrSel pos' em) (Object tyName (ValueStr str))

applyActionM (ActionMoveStrCursorForward path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos em = nodeSel
    guard (pos < Text.length str)
    let pos' = pos + 1
    put $ Node (NodeStrSel pos' em) (Object tyName (ValueStr str))

applyActionM (ActionInsertLetter path c) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueStr str)) <- get
    let NodeStrSel pos editMode = nodeSel
    guard editMode
    let (before, after) = Text.splitAt pos str
        str' = before <> Text.singleton c <> after
        pos' = pos + 1
    put $ Node (NodeStrSel pos' editMode) (Object tyName (ValueStr str'))
    setUndoFlag

applyActionM (ActionSelectParent path) = do
  path' <- maybeA (pathParent path)
  zoom (atPath path') $ do
    Node nodeSel (Object tyName (ValueRec fields)) <- get
    let NodeRecSel recSel = nodeSel
    let recSel' = toRecSelSelf recSel
    put $ Node (NodeRecSel recSel') (Object tyName (ValueRec fields))

applyActionM (ActionSelectChild path) =
  zoom (atPath path) $ do
    Node nodeSel (Object tyName (ValueRec fields)) <- get
    let NodeRecSel recSel = nodeSel
    recSel' <- maybeA (toRecSelChild recSel)
    put $ Node (NodeRecSel recSel') (Object tyName (ValueRec fields))

applyActionM (ActionSelectSiblingBackward path) = do
  path' <- maybeA (pathParent path)
  zoomPathPrefix path' $ do
    Node nodeSel (Object tyName (ValueRec fields)) <- get
    let NodeRecSel recSel = nodeSel
    RecSel fieldName True <- pure recSel
    recMoveMaps <- view rctxRecMoveMaps
    let moveMap = rmmBackward (recMoveMaps HashMap.! tyName)
    fieldName' <- maybeA (HashMap.lookup fieldName moveMap)
    let recSel' = RecSel fieldName' True
    put $ Node (NodeRecSel recSel') (Object tyName (ValueRec fields))

applyActionM (ActionSelectSiblingForward path) = do
  path' <- maybeA (pathParent path)
  zoomPathPrefix path' $ do
    Node nodeSel (Object tyName (ValueRec fields)) <- get
    let NodeRecSel recSel = nodeSel
    RecSel fieldName True <- pure recSel
    recMoveMaps <- view rctxRecMoveMaps
    let moveMap = rmmForward (recMoveMaps HashMap.! tyName)
    fieldName' <- maybeA (HashMap.lookup fieldName moveMap)
    let recSel' = RecSel fieldName' True
    put $ Node (NodeRecSel recSel') (Object tyName (ValueRec fields))

pathTip :: Path -> Maybe PathSegment
pathTip (Path ps) = listToMaybe (List.reverse ps)

pathParent :: Path -> Maybe Path
pathParent (Path ps) =
  case List.reverse ps of
    [] -> Nothing
    _:ps' -> Just (Path (List.reverse ps'))

atPath :: Path -> Traversal' Node Node
atPath p =
  case unconsPath p of
    Nothing -> id
    Just (ps, p') -> atPathSegment ps . atPath p'

atPathSegment :: PathSegment -> Traversal' Node Node
atPathSegment (PathSegmentSeq _ _) =
  error "TODO (int-index): updatePathRec PathSegmentSeq"
atPathSegment (PathSegmentRec tyName fieldName) =
  \f node ->
    case node of
      Node nodeSel (Object tyName' value)
        | tyName == tyName',
          ValueRec fields <- value,
          let NodeRecSel recSel = nodeSel,
          Just a <- HashMap.lookup fieldName fields
        -> f a <&> \a' ->
             let fields' = (HashMap.insert fieldName a' fields)
             in Node (NodeRecSel recSel) (Object tyName' (ValueRec fields'))
      _ -> pure node

zoomPathPrefix :: Path -> ReactM -> ReactM
zoomPathPrefix p m =
  case unconsPath p of
    Nothing -> m
    Just (ps, p') ->
      zoom (atPathSegment ps) (zoomPathPrefix p' m) <|> m

mkDefaultNodes :: Env -> HashMap TyName RecMoveMap -> HashMap TyName Node
mkDefaultNodes env recMoveMaps =
  HashMap.mapWithKey mkDefNode (envMap env)
  where
    mkDefNode :: TyName -> Ty -> Node
    mkDefNode tyName = \case
      TyStr -> Node (NodeStrSel 0 True) (Object tyName (ValueStr ""))
      TySeq _ -> error "TODO (int-index): mkDefNode TySeq"
      TyRec fieldTys ->
        let
          fields = HashMap.map (const Hole) fieldTys
          recMoveMap = recMoveMaps HashMap.! tyName
          recSel =
            case rmmFieldOrder recMoveMap of
              [] -> RecSel0
              fieldName:_ -> RecSel fieldName True
        in
          Node (NodeRecSel recSel) (Object tyName (ValueRec fields))

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
      _pluginInfoDefaultNodes :: HashMap TyName Node,
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
      _pluginInfoDefaultNodes = defaultNodes,
      _pluginInfoAllowedFieldTypes = allowedFieldTypes
    }
  where
    tyEnv = plugin ^. pluginTyEnv
    recLayouts = plugin ^. pluginRecLayouts
    nodeFactory = plugin ^. pluginNodeFactory
    recMoveMaps = mkRecMoveMaps recLayouts
    defaultNodes = mkDefaultNodes tyEnv recMoveMaps
    allowedFieldTypes = mkAllowedFieldTypes tyEnv
