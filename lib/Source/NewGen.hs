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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Source.NewGen
  (
  -- * Names
  TyName,
  FieldName,

  -- * Types
  Schema(..),
  Ty(..),
  TyUnion(..),
  mkTyUnion,

  -- * Values
  Node(..),
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
  StackVis(..),
  Selection(..),
  Paths(..),
  Ann,
  El,
  redrawUI,
  cairoPositionedElementRender,
  Find(..),
  PrecPredicate,
  precAllow,
  precAllowAll,
  Layout(vsep, field, jumptag),
  noPrec,
  ALayoutFn(..),
  WritingDirection(..),

  -- * React
  ReactResult(..),

  -- * Editor
  EditorState(..),
  initEditorState,
  fromParsedValue,
  esExpr,
  esPointer,
  esPrecBordersAlways,
  esWritingDirection,
  esStack,
  esStackVis,
  esUndo,
  esRedo,
  esRenderUI,
  esPointerPath,
  esJumptags,
  esActiveJumptags,

  RecMoveMap,

  selectionOfEditorState,
  reactEditorState,

  -- * Plugin
  Plugin(..),

  PluginInfo,
  mkPluginInfo,

  -- * Utils
  inj,
  nothing,
  maybeA

  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Sequence as Seq
import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Foldable as Foldable
import Data.DList as DList
import Data.Semigroup
import Control.Applicative as A
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens as Lens hiding (elements, Index)
import Data.Function (on)
import Data.String
import Inj
import Inj.Base ()

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Graphics.Rendering.Cairo as Cairo (Render)

import Slay.Core
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Combinators
import Slay.Cairo.Element

import Source.Input
import qualified Source.Input.KeyCode as KeyCode

import Sdam.Core
import Sdam.Name
import Sdam.Validator
import Sdam.Parser

mkTyUnion :: [TyName] -> Maybe TyUnion -> TyUnion
mkTyUnion xs = TyUnion (HashSet.fromList xs)

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data Node = Hole | Node NodeSel (Value Node)

data NodeSel =
  NodeRecSel RecSel |
  NodeSeqSel SeqSel |
  NodeStrSel Int Bool   -- True = edit mode

data RecSel =
  -- for records without children
  RecSel0 |
  -- for records with children
  RecSel FieldName Bool   -- True = child selected

data SeqSel =
  -- for empty sequences
  SeqSel0 |
  -- for non-empty sequences
  SeqSel Index Bool   -- True = child selected

--------------------------------------------------------------------------------
---- Validation
--------------------------------------------------------------------------------

toValidationValue :: Node -> ValidationValue
toValidationValue Hole = SkipValidation
toValidationValue (Node _ value) =
  ValidationValue (fmap toValidationValue value)

validateNode :: Schema -> Node -> ValidationResult
validateNode schema node = validate schema (toValidationValue node)

--------------------------------------------------------------------------------
---- Drawing
--------------------------------------------------------------------------------

data CursorBlink = CursorVisible | CursorInvisible

blink :: CursorBlink -> CursorBlink
blink = \case
  CursorVisible -> CursorInvisible
  CursorInvisible -> CursorVisible

data StackVis = StackVisible | StackHidden

data SelectionTip =
  SelectionTipLabeled TyName |
  SelectionTipSeq

data Selection =
  Selection
    { selectionPath :: Path,
      selectionTip :: Maybe SelectionTip,
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

textline ::
  Inj (CairoElement DrawCtx) a =>
  Color -> Font -> Text -> (Paths -> CursorBlink -> Maybe Natural) -> a
textline color font str cur = text font (inj color) str (DrawCtx cur)

line :: Color -> Natural -> Collage Ann El
line color w = rect nothing (inj color) (Extents w 1)

vline :: Color -> Natural -> Collage Ann El
vline color h = rect nothing (inj color) (Extents 1 h)

leftOf :: Collage n El -> Integer
leftOf collage = toInteger (marginLeft (collageMargin collage))

rightOf :: Extents -> Collage n El -> Integer
rightOf (Extents vacantWidth _) collage =
  let
    Extents width _ = collageExtents collage
    mRight =  marginRight (collageMargin collage)
    minus a b = max 0 (toInteger a - toInteger b)
  in
    vacantWidth `minus` (width + mRight)

data WritingDirection = WritingDirectionLTR | WritingDirectionRTL

infixr 1 `vsep`

class (IsString a, Semigroup a) => Layout a where
  vsep :: a -> a -> a
  field :: FieldName -> PrecPredicate -> a
  jumptag :: a -> a

noPrec :: PrecPredicate
noPrec = PrecPredicate (const (PrecBorder True))

newtype RecLayoutFn =
  RecLayoutFn {
    appRecLayoutFn ::
      Path ->
      HashMap FieldName (PrecPredicate -> (PrecUnenclosed, Collage Ann El)) ->
      WritingDirection ->
      (PrecUnenclosed, Collage Ann El)
  }

instance IsString RecLayoutFn where
  fromString s =
    RecLayoutFn $ \_ _ _ ->
      (mempty, punct (fromString s))

instance Semigroup RecLayoutFn where
  RecLayoutFn a <> RecLayoutFn b =
    RecLayoutFn $ \path m wd ->
      let
        (aUnenclosed, a') = a path m wd
        (bUnenclosed, b') = b path m wd
        f = case wd of
          WritingDirectionLTR -> horizBaseline
          WritingDirectionRTL -> flip horizBaseline
      in
        (,) (aUnenclosed <> bUnenclosed) $
        f a' b'

instance Layout RecLayoutFn where
  RecLayoutFn a `vsep` RecLayoutFn b =
    RecLayoutFn $ \path m wd ->
      let
        (aUnenclosed, a') = a path m wd
        (bUnenclosed, b') = b path m wd
        f = case wd of
          WritingDirectionLTR -> vertLeft
          WritingDirectionRTL -> vertRight
        maxWidth = (max `on` widthOf) a' b'
      in
        (,) (aUnenclosed <> bUnenclosed) $
        a' `f` line light1 maxWidth `f` b'
  field fieldName precPredicate =
    RecLayoutFn $ \_ m _ ->
      (m HashMap.! fieldName) precPredicate
  jumptag (RecLayoutFn a) =
    RecLayoutFn $ \path m wd ->
      let (aUnenclosed, a') = a path m wd
      in  (aUnenclosed, withJumptag path a')

withJumptag :: Path -> Collage Ann El -> Collage Ann El
withJumptag path =
    collageAnnotate (\o -> (mempty, mempty, pathJumptag o))
  where
    pathJumptag offset = DList.singleton (Jumptag offset path)

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
  jumptag (ALayoutFn a) = ALayoutFn (jumptag a)

newtype Find a b = Find (a -> Maybe b)

instance Semigroup (Find a b) where
  Find f1 <> Find f2 =
    Find $ \a -> f1 a <|> f2 a

instance Monoid (Find a b) where
  mempty = Find (const Nothing)

type FindPath = Find Offset Path
type FindZone = Find Path (Offset, Extents)

data Jumptag = Jumptag Offset Path

-- Collage elements:
type El = CairoElement DrawCtx

-- Collage annotations:
type Ann = (FindPath, FindZone, DList Jumptag)

getNoAnn :: ((), a) -> a
getNoAnn = snd

renderSelectionBorder :: Paths -> FindZone -> CairoRender DrawCtx
renderSelectionBorder Paths{pathsSelection} (Find findZone) =
  case findZone (selectionPath pathsSelection) of
    Just (o, e) ->
      getNoAnn $
      foldCairoCollage o $
      outline 2 selectionBorderColor e
    Nothing -> mempty

renderHoverBorder :: Paths -> FindZone -> CairoRender DrawCtx
renderHoverBorder Paths{pathsCursor} (Find findZone) =
  case pathsCursor of
    Just p | Just (o, e) <- findZone p ->
      getNoAnn $
      foldCairoCollage o $
      outline 2 hoverBorderColor e
    _ -> mempty

renderJumptagLabels :: [(Char, Jumptag)] -> CairoRender DrawCtx
renderJumptagLabels = foldMap renderJumptagLabel

renderJumptagLabel :: (Char, Jumptag) -> CairoRender DrawCtx
renderJumptagLabel (c, Jumptag o _) =
    getNoAnn $
    foldCairoCollage o $
    substrate 1 (rect nothing dark2) $
    textline (rgb 255 127 80) ubuntuMonoFont label nothing
  where
    label = Text.toUpper (Text.singleton c)

renderMotion :: Motion -> Paths -> FindZone -> CairoRender DrawCtx
renderMotion motion Paths{pathsSelection} (Find findZone) =
  case findZone (selectionPath pathsSelection) of
    Nothing -> error "renderMotion: no selection"
    Just (o, e) ->
      getNoAnn $
      foldCairoCollage o $
      mapCollageAnnotation (const mempty) $
      substrate 0 (rect nothing dark1') $
      substrateMargin (outline 2 motionBorderColor . extentsMax e) $
      collageWithMargin (Margin 4 4 4 4) $
      (punct label :: Collage Ann El)
  where
    Motion label = motion

findPathInBox :: Path -> (Offset, Extents) -> FindPath
findPathInBox p box =
  Find $ \point ->
    if insideBox box point
    then Just p
    else Nothing

findBoxAtPath :: Path -> (Offset, Extents) -> FindZone
findBoxAtPath p box =
  Find $ \p' ->
    if p == p'
    then Just box
    else Nothing

dark1, dark1', dark2, light1, white, red :: Inj Color a => a
dark1  = grayscale 41
dark1' = grayscale 35
dark2  = grayscale 77
light1 = grayscale 179
white  = grayscale 255
red    = rgb 255 0 0

selectionBorderColor,
  hoverBorderColor,
  stackBorderColor,
  motionBorderColor :: Inj Color a => a
selectionBorderColor = rgb 94 80 134
hoverBorderColor = rgb 255 127 80
stackBorderColor = rgb 45 134 108
motionBorderColor = rgb 45 134 108

textWithCursor :: Text -> (Paths -> CursorBlink -> Maybe Natural) -> Collage Ann El
textWithCursor = textline white ubuntuFont

textWithoutCursor :: Text -> Collage Ann El
textWithoutCursor t = textWithCursor t nothing

outline ::
  Inj (CairoElement DrawCtx) a =>
  Natural -> DrawCtx (Maybe Color) -> Extents -> a
outline width = rect (inj (pure width :: LRTB Natural))

punct ::
  Inj (CairoElement DrawCtx) a =>
  Text -> a
punct t = textline light1 ubuntuFont t nothing

ubuntuFont :: Font
ubuntuFont = Font "Ubuntu" 12 FontWeightNormal

ubuntuMonoFont :: Font
ubuntuMonoFont = Font "Ubuntu Mono" 12 FontWeightNormal

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

precAllowAll :: PrecPredicate
precAllowAll = PrecPredicate (const (PrecBorder False))

hashSet_isSubsetOf :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
hashSet_isSubsetOf sub sup =
  all (\k -> HashSet.member k sup) sub

lrtbMargin :: Margin -> LRTB Natural
lrtbMargin (Margin l r t b) = lrtb l r t b

substrateMargin ::
  Semigroup n =>
  (Extents -> Collage n a) ->
  Collage n a ->
  Collage n a
substrateMargin f a =
  substrate (lrtbMargin (collageMargin a)) f a

foldCairoCollage :: Offset -> Collage n (CairoElement g) -> (n, CairoRender g)
foldCairoCollage = foldMapCollage cairoPositionedElementRender

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
maybeA = maybe A.empty A.pure

alwaysSucceed :: Alternative f => f () -> f ()
alwaysSucceed f = f <|> pure ()

--------------------------------------------------------------------------------
---- Editor
--------------------------------------------------------------------------------

data Motion = Motion Text

data EditorState =
  EditorState
    { _esExpr :: Node,
      _esPointer :: Offset,
      _esPrecBordersAlways :: Bool,
      _esWritingDirection :: WritingDirection,
      _esStack :: [Node],
      _esStackVis :: StackVis,
      _esUndo :: [Node],
      _esRedo :: [Node],
      _esRenderUI :: CursorBlink -> Cairo.Render (),
      _esPointerPath :: Maybe Path,
      _esJumptags :: [Jumptag],
      _esActiveJumptags :: [(Char, Jumptag)],
      _esMotion :: Maybe Motion
    }

initEditorState :: EditorState
initEditorState =
  EditorState
    { _esExpr = Hole,
      _esPointer = offsetZero,
      _esPrecBordersAlways = False,
      _esWritingDirection = WritingDirectionLTR,
      _esStack = [],
      _esStackVis = StackHidden,
      _esUndo = [],
      _esRedo = [],
      _esRenderUI = const (pure ()),
      _esPointerPath = Nothing,
      _esJumptags = [],
      _esActiveJumptags = [],
      _esMotion = Nothing
    }

data LayoutCtx =
  LayoutCtx
    { _lctxPath :: PathBuilder,
      _lctxValidationResult :: ValidationResult,
      _lctxViewport :: Extents,
      _lctxPrecBordersAlways :: Bool,
      _lctxRecLayouts :: HashMap TyName ALayoutFn,
      _lctxWritingDirection :: WritingDirection
    }

data ReactCtx =
  ReactCtx
    { _rctxSchema :: Schema,
      _rctxDefaultNodes :: HashMap TyName Node,
      _rctxRecMoveMaps :: HashMap TyName RecMoveMap,
      _rctxJumptags :: [Jumptag]
    }

data RecMoveMap =
  RecMoveMap
    { rmmFieldOrder :: [FieldName],
      rmmForward :: HashMap FieldName FieldName,
      rmmBackward :: HashMap FieldName FieldName
    }

data ReactState =
  ReactState
    { _rstNode :: Node,
      _rstStack :: [Node],
      _rstStackVis :: StackVis,
      _rstActiveJumptags :: [(Char, Jumptag)],
      _rstMotion :: Maybe Motion
    }

--------------------------------------------------------------------------------
---- Lenses
--------------------------------------------------------------------------------

makeLenses ''EditorState
makeLenses ''LayoutCtx
makeLenses ''ReactCtx
makeLenses ''ReactState

--------------------------------------------------------------------------------
---- Utils
--------------------------------------------------------------------------------

keyLetter :: Char -> KeyCode -> Bool
keyLetter c keyCode = keyChar keyCode == Just c

keyCodeLetter :: KeyCode -> Char -> InputEvent -> Bool
keyCodeLetter kc c = \case
  KeyPress [] keyCode -> keyCode == kc || keyLetter c keyCode
  _ -> False

--------------------------------------------------------------------------------
---- Editor - Layout
--------------------------------------------------------------------------------

redrawUI ::
  PluginInfo ->
  Extents ->
  EditorState ->
  EditorState
redrawUI pluginInfo viewport es =
  es & esRenderUI .~ renderUI
     & esPointerPath .~ pathsCursor
     & esJumptags .~ jumptags
  where
    lctx =
      LayoutCtx
        { _lctxPath = mempty @PathBuilder,
          _lctxValidationResult = mempty, -- initialized in layoutNodeStack
                                          --            and layoutMainExpr
          _lctxViewport = viewport,
          _lctxPrecBordersAlways = es ^. esPrecBordersAlways,
          _lctxRecLayouts = pluginInfoRecLayouts pluginInfo,
          _lctxWritingDirection = es ^. esWritingDirection
        }
    schema = pluginInfoSchema pluginInfo
    pointer = es ^. esPointer
    activeJumptags = es ^. esActiveJumptags
    infoBarLayout = layoutInfoBar es
    stackLayout = layoutNodeStack schema lctx (es ^. esStack)
    mainLayout = layoutMainExpr schema lctx (es ^. esExpr)
    hOff :: Collage n a -> Integer
    hOff c =
      toInteger (heightOf infoBarLayout) +
      toInteger (marginTop (collageMargin c)) +
      10
    mainOffset =
      Offset { offsetX = leftOf mainLayout + 10,
               offsetY = hOff mainLayout }
    stackOffset =
      Offset { offsetX = rightOf (lctx ^. lctxViewport) stackLayout,
               offsetY = hOff stackLayout }
    ((), backgroundRdr) =
      foldCairoCollage offsetZero $
      rect nothing dark1 (lctx ^. lctxViewport)
    ((), barBackgroundRdr) =
      foldCairoCollage offsetZero $
      rect nothing selectionBorderColor $
      Extents (extentsW (lctx ^. lctxViewport)) (heightOf infoBarLayout)
    ((Find findPath, findZone, jumptags'), mainRdr) =
      foldCairoCollage mainOffset mainLayout
    ((), stackRdr') =
      foldCairoCollage stackOffset stackLayout
    ((), infoBarRdr) =
      foldCairoCollage offsetZero infoBarLayout
    jumptags = DList.toList jumptags'
    stackRdr = case es ^. esStackVis of
      StackHidden -> mempty
      StackVisible -> stackRdr'
    pathsCursor = findPath pointer
    pathsSelection = selectionOfEditorState es
    paths = Paths {pathsCursor, pathsSelection}
    renderUI cursorBlink =
      cairoRender
        (backgroundRdr
           <> mainRdr
           <> (case es ^. esMotion of
                 Nothing -> renderSelectionBorder paths findZone
                 Just a -> renderMotion a paths findZone)
           <> renderHoverBorder paths findZone
           <> renderJumptagLabels activeJumptags
           <> stackRdr
           <> barBackgroundRdr
           <> infoBarRdr)
        (withDrawCtx paths cursorBlink)

layoutMainExpr ::
  Schema ->
  LayoutCtx ->
  Node ->
  Collage Ann El
layoutMainExpr schema lctx expr = collage
  where
    lctx' n = lctx{ _lctxValidationResult = validateNode schema n }
    (_, collage) = layoutNode (lctx' expr) expr precAllowAll

layoutNodeStack :: Monoid n => Schema -> LayoutCtx -> [Node] -> Collage n El
layoutNodeStack schema lctx nodes =
  mapCollageAnnotation (const mempty) $
  substrate 0 backgroundRect $
  substrate 4 (outline 2 stackBorderColor) $
  case nodes of
    [] -> punct "end of stack"
    n:_ -> snd (layoutNode (lctx' n) n precAllowAll)
  where
    lctx' n = lctx{ _lctxValidationResult = validateNode schema n }
    backgroundRect = rect nothing dark1'

layoutInfoBar ::
  Monoid n =>
  EditorState ->
  Collage n El
layoutInfoBar es =
  mapCollageAnnotation (const mempty) $
  textWithoutCursor (pprSelection (selectionOfEditorState es))

pprSelection :: Selection -> Text
pprSelection selection = Text.pack (goPath selectionPath "")
  where
    Selection{selectionPath, selectionTip, selectionStrPos} = selection
    goPath p =
      case unconsPath p of
        Nothing -> goTip selectionTip
        Just (ps, p') -> goPathSegment ps . ('→':) . goPath p'
    goTip Nothing = ('_':)
    goTip (Just (SelectionTipLabeled tyName)) =
      (tyNameStr tyName++) . goStrPos selectionStrPos
    goTip (Just SelectionTipSeq) = ('∗':)
    goPathSegment ps =
      case ps of
        PathSegmentRec tyName fieldName ->
          (tyNameStr tyName++) . ('.':) . (fieldNameStr fieldName++)
        PathSegmentSeq i ->
          shows (indexToInt i)
    goStrPos Nothing = id
    goStrPos (Just i) = ('[':) . shows i . (']':)

layoutNode :: LayoutCtx -> Node -> PrecPredicate -> (PrecUnenclosed, Collage Ann El)
layoutNode lctx = \case
  Hole -> \_precPredicate -> layoutHole lctx
  Node _ value -> layoutValue lctx value

layoutHole :: LayoutCtx -> (PrecUnenclosed, Collage Ann El)
layoutHole lctx =
  (,) (mempty @PrecUnenclosed) $
  layoutSel (BorderValid precBorder) path $
  withJumptag path $
  punct "_"
  where
    precBorder = PrecBorder (lctx ^. lctxPrecBordersAlways)
    path = buildPath (lctx ^. lctxPath)

layoutValue ::
  LayoutCtx ->
  Value Node ->
  PrecPredicate ->
  (PrecUnenclosed, Collage Ann El)
layoutValue lctx = \case
  ValueRec tyName fields ->
    layoutRec lctx tyName fields
  ValueSeq items ->
    layoutSeq lctx items
  ValueStr _ str -> \_precPredicate ->
    layoutStr lctx str

layoutStr ::
  LayoutCtx ->
  Text ->
  (PrecUnenclosed, Collage Ann El)
layoutStr lctx str =
  (,) (mempty @PrecUnenclosed) $
  layoutSel (toBorder lctx precBorder) path $
  withJumptag path $
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

lctxDescent :: PathSegment -> LayoutCtx -> LayoutCtx
lctxDescent pathSegment lctx =
  lctx & lctxPath %~ (<> mkPathBuilder pathSegment)
       & lctxValidationResult %~ pathTrieLookup pathSegment

layoutRec ::
  LayoutCtx ->
  TyName ->
  HashMap FieldName Node ->
  PrecPredicate ->
  (PrecUnenclosed, Collage Ann El)
layoutRec lctx tyName fields precPredicate =
  (,) (guardUnenclosed precBorder precUnenclosed') $
  layoutSel (toBorder lctx precBorder) path $
  collage
  where
    layoutFields :: RecLayoutFn
    layoutFields =
      case HashMap.lookup tyName (lctx ^. lctxRecLayouts) of
        Nothing -> fromString (show tyName)
        Just (ALayoutFn fn) -> fn
    drawnFields :: HashMap FieldName (PrecPredicate -> (PrecUnenclosed, Collage Ann El))
    drawnFields =
      HashMap.mapWithKey
        (\fieldName node ->
          let lctx' = lctxDescent (PathSegmentRec tyName fieldName) lctx
          in layoutNode lctx' node)
        fields
    (precUnenclosed, collage) =
      appRecLayoutFn layoutFields path drawnFields wd
    precUnenclosed' = addUnenclosed tyName precUnenclosed
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways) <>
      appPrecPredicate precPredicate precUnenclosed'
    path = buildPath (lctx ^. lctxPath)
    wd = lctx ^. lctxWritingDirection

layoutSeq ::
  LayoutCtx ->
  Seq Node ->
  PrecPredicate ->
  (PrecUnenclosed, Collage Ann El)
layoutSeq lctx items precPredicate =
  (,) (guardUnenclosed precBorder precUnenclosed') $
  layoutSel (toBorder lctx precBorder) path $
  collage
  where
    drawnItems :: [PrecPredicate -> (PrecUnenclosed, Collage Ann El)]
    drawnItems =
      List.zipWith
        (\i node ->
          let lctx' = lctxDescent (PathSegmentSeq i) lctx
          in layoutNode lctx' node)
        (List.map intToIndex [0..])
        (Foldable.toList items)
    (precUnenclosed, collage) =
      layoutSeqItems path drawnItems wd
    precUnenclosed' =
      -- TODO (int-index): add a sequence marker when non-empty
      precUnenclosed
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways) <>
      appPrecPredicate precPredicate precUnenclosed'
    path = buildPath (lctx ^. lctxPath)
    wd = lctx ^. lctxWritingDirection

layoutSeqItems ::
  Path ->
  [PrecPredicate -> (PrecUnenclosed, Collage Ann El)] ->
  WritingDirection ->
  (PrecUnenclosed, Collage Ann El)
layoutSeqItems path xs =
  case nonEmpty xs of
    Nothing -> \_wd -> (mempty, withJumptag path (punct "∅"))
    Just xs' -> layoutSeqItems' xs'

layoutSeqItems' ::
  NonEmpty (PrecPredicate -> (PrecUnenclosed, Collage Ann El)) ->
  WritingDirection ->
  (PrecUnenclosed, Collage Ann El)
layoutSeqItems' xs wd =
  let
    f = case wd of
      WritingDirectionLTR -> vertLeft
      WritingDirectionRTL -> vertRight
    g = case wd of
      WritingDirectionLTR -> horizTop
      WritingDirectionRTL -> flip horizTop
    addB x = vline dark2 (heightOf x) `g` x
    (xsUnenclosed, xs') = NonEmpty.unzip (fmap ($ precAllowAll) xs)
  in
    (,) (sconcat xsUnenclosed) $
    nefoldr1 f (fmap addB xs')

nefoldr1 :: (a -> a -> a) -> NonEmpty a -> a
nefoldr1 = List.foldr1 -- safe for non-empty lists

data Border = BorderValid PrecBorder | BorderInvalid

layoutSel :: Border -> Path -> Collage Ann El -> Collage Ann El
layoutSel border path =
  collageWithMargin (mkMargin (marginWidth - borderWidth)) .
  collageAnnotateMargin pathZone .
  layoutBorder borderWidth border .
  collageWithMargin (mkMargin marginWidth)
  where
    mkMargin a = Margin a a a a
    (marginWidth, borderWidth) = (4, 1)
    pathZone box = (findPath, findZone, mempty)
      where
        findPath = findPathInBox path box
        findZone = findBoxAtPath path box

layoutBorder :: Natural -> Border -> Collage Ann El -> Collage Ann El
layoutBorder borderWidth = \case
    BorderInvalid -> addBorder red
    BorderValid (PrecBorder True) -> addBorder dark2
    BorderValid (PrecBorder False) -> id
  where
    addBorder color =
      substrateMargin (outline borderWidth color)

toBorder :: LayoutCtx -> PrecBorder -> Border
toBorder lctx
    | validChild = BorderValid
    | otherwise = const BorderInvalid
  where
    validChild =
      HashSet.null (pathTrieRoot (lctx ^. lctxValidationResult))

--------------------------------------------------------------------------------
---- Editor - Selection
--------------------------------------------------------------------------------

selectionOfEditorState :: EditorState -> Selection
selectionOfEditorState es = selectionOfNode (es ^. esExpr)

selectionOfNode :: Node -> Selection
selectionOfNode = \case
  Hole -> Selection emptyPath Nothing Nothing
  Node nodeSel (ValueStr tyName _) ->
    let NodeStrSel pos em = nodeSel
    in Selection emptyPath
        (Just (SelectionTipLabeled tyName))
        (if em then Just pos else Nothing)
  Node nodeSel (ValueSeq items) ->
    let NodeSeqSel seqSel = nodeSel in
    case seqSel of
      SeqSel0 ->
        Selection emptyPath (Just SelectionTipSeq) Nothing
      SeqSel _ False ->
        Selection emptyPath (Just SelectionTipSeq) Nothing
      SeqSel i True ->
        let
          pathSegment = PathSegmentSeq i
          seqItem = Seq.index items (indexToInt i)
          Selection pathTail tip' strPos =
            selectionOfNode seqItem
        in
          Selection (consPath pathSegment pathTail) tip' strPos
  Node nodeSel (ValueRec tyName fields) ->
    let NodeRecSel recSel = nodeSel in
    case recSel of
      RecSel0 ->
        Selection emptyPath (Just (SelectionTipLabeled tyName)) Nothing
      RecSel _ False ->
        Selection emptyPath (Just (SelectionTipLabeled tyName)) Nothing
      RecSel fieldName True ->
        let
          pathSegment = PathSegmentRec tyName fieldName
          recField = fields HashMap.! fieldName
          Selection pathTail tip' strPos =
            selectionOfNode recField
        in
          Selection (consPath pathSegment pathTail) tip' strPos

-- | Set self-selection for all nodes.
resetPathNode :: Node -> Node
resetPathNode node =
  case node of
    Hole -> node
    Node sel value -> Node (resetSel sel) (fmap resetPathNode value)
  where
    resetSel (NodeRecSel recSel) = NodeRecSel (toRecSelSelf recSel)
    resetSel (NodeSeqSel seqSel) = NodeSeqSel (toSeqSelSelf seqSel)
    resetSel s@(NodeStrSel _ _) = s

updatePathNode :: Path -> Node -> Node
updatePathNode path node = case node of
  Hole -> node
  Node _ (ValueStr _ _) -> node
  Node nodeSel (ValueSeq items) ->
    let NodeSeqSel seqSel = nodeSel in
    case unconsPath path of
      Nothing ->
        let seqSel' = toSeqSelSelf seqSel
        in Node (NodeSeqSel seqSel') (ValueSeq items)
      Just (PathSegmentRec _ _, _) -> node
      Just (PathSegmentSeq i, path') ->
        let i' = indexToInt i in
        case Seq.lookup i' items of
          Nothing -> node
          Just a ->
            let
              a' = updatePathNode path' a
              items' = Seq.update i' a' items
              seqSel' = SeqSel i True
            in
              Node (NodeSeqSel seqSel') (ValueSeq items')
  Node nodeSel (ValueRec tyName fields) ->
    let NodeRecSel recSel = nodeSel in
    case unconsPath path of
      Nothing ->
        let recSel' = toRecSelSelf recSel
        in Node (NodeRecSel recSel') (ValueRec tyName fields)
      Just (PathSegmentSeq _, _) -> node
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
              Node (NodeRecSel recSel') (ValueRec tyName fields')

setPathNode :: Path -> Node -> Node
setPathNode path node = updatePathNode path (resetPathNode node)

toRecSelSelf :: RecSel -> RecSel
toRecSelSelf RecSel0 = RecSel0
toRecSelSelf (RecSel fieldName _) = RecSel fieldName False

toRecSelChild :: RecSel -> Maybe RecSel
toRecSelChild RecSel0 = Nothing
toRecSelChild (RecSel fieldName _) = Just (RecSel fieldName True)

toSeqSelSelf :: SeqSel -> SeqSel
toSeqSelSelf SeqSel0 = SeqSel0
toSeqSelSelf (SeqSel i _) = SeqSel i False

toSeqSelChild :: SeqSel -> Maybe SeqSel
toSeqSelChild SeqSel0 = Nothing
toSeqSelChild (SeqSel i _) = Just (SeqSel i True)

toNodeSelSelf :: NodeSel -> Maybe NodeSel
toNodeSelSelf (NodeRecSel recSel) = Just (NodeRecSel (toRecSelSelf recSel))
toNodeSelSelf (NodeSeqSel seqSel) = Just (NodeSeqSel (toSeqSelSelf seqSel))
toNodeSelSelf (NodeStrSel _ _) = Nothing

toNodeSelChild :: NodeSel -> Maybe NodeSel
toNodeSelChild (NodeRecSel recSel) = NodeRecSel <$> toRecSelChild recSel
toNodeSelChild (NodeSeqSel seqSel) = NodeSeqSel <$> toSeqSelChild seqSel
toNodeSelChild (NodeStrSel _ _) = Nothing

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

reactEditorState :: PluginInfo -> InputEvent -> EditorState -> ReactResult EditorState

reactEditorState _ (PointerMotion x y) es = ReactOk $
  es & esPointer .~ Offset (fromIntegral x) (fromIntegral y)

reactEditorState _ ButtonPress es
  | Just p <- es ^. esPointerPath
  = ReactOk $ es & esExpr %~ setPathNode p

reactEditorState _ (KeyPress [Control] keyCode) es
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

reactEditorState pluginInfo inputEvent es
  | Just act <- getAction schema wd sel stkVis activeJumptags motion inputEvent,
    Just (UndoFlag undoFlag, rst') <- applyAction act rctx rst
  = ReactOk $
    let es' = es & esExpr .~ (rst' ^. rstNode)
                 & esStack .~ (rst' ^. rstStack)
                 & esStackVis .~ (rst' ^. rstStackVis)
                 & esActiveJumptags .~ (rst' ^. rstActiveJumptags)
                 & esMotion .~ (rst' ^. rstMotion)
    in if undoFlag then
         es' & esUndo %~ (expr:)
             & esRedo .~ []
       else es'
  where
    schema = rctx ^. rctxSchema
    wd = es ^. esWritingDirection
    sel = selectionOfNode expr
    expr = es ^. esExpr
    stkVis = es ^. esStackVis
    activeJumptags = es ^. esActiveJumptags
    motion = es ^. esMotion
    rst = ReactState expr (es ^. esStack) stkVis activeJumptags motion
    rctx =
      ReactCtx
        { _rctxSchema = pluginInfoSchema pluginInfo,
          _rctxDefaultNodes = pluginInfoDefaultNodes pluginInfo,
          _rctxRecMoveMaps = pluginInfoRecMoveMaps pluginInfo,
          _rctxJumptags = es ^. esJumptags
        }

reactEditorState _ _ _ = UnknownEvent

data Action =
  ActionDeleteNode Path |
  ActionPushStack Path |
  ActionPopSwapStack Path |
  ActionRotateStack |
  ActionDropStack |
  ActionExitEditMode Path |
  ActionDeleteCharBackward Path |
  ActionDeleteCharForward Path |
  ActionMoveStrCursorBackward Path |
  ActionMoveStrCursorForward Path |
  ActionInsertLetter Path Char |
  ActionAppendSeqItem Path |
  ActionSelectParent Path |
  ActionSelectChild Path |
  ActionSelectSiblingBackward Path |
  ActionSelectSiblingForward Path |
  ActionActivateJumptags |
  ActionDeactivateJumptags |
  ActionJumptagLookup Char |
  ActionStartMotion |
  ActionAppendMotion Char |
  ActionCommitMotion Path

getAction ::
  Schema ->
  WritingDirection ->
  Selection ->
  StackVis ->
  [(Char, Jumptag)] ->
  Maybe Motion ->
  InputEvent ->
  Maybe Action
getAction
    Schema{schemaTypes}
    wd
    Selection{selectionPath, selectionTip, selectionStrPos}
    stkVis
    activeJumptags
    motion
    inputEvent

  -- Exit jumptag mode.
  | not (Foldable.null activeJumptags),
    KeyPress [] KeyCode.Escape <- inputEvent
  = Just $ ActionDeactivateJumptags

  -- Enter motion mode.
  | Nothing <- motion,
    KeyPress [] KeyCode.Space <- inputEvent
  = return ActionStartMotion

  -- Append a letter to the motion.
  | Just _ <- motion,
    KeyPress mods keyCode <- inputEvent,
    Control `notElem` mods,
    Just c <- keyChar keyCode
  = Just $ ActionAppendMotion c

  -- Commit a motion.
  | Just _ <- motion,
    KeyRelease _ KeyCode.Space <- inputEvent
  = Just $ ActionCommitMotion selectionPath

  -- Jumptag lookup.
  | not (Foldable.null activeJumptags),
    KeyPress [] keyCode <- inputEvent,
    Just c <- keyChar keyCode
  = Just $ ActionJumptagLookup c

  -- Exit edit mode.
  | Just (SelectionTipLabeled tyName) <- selectionTip,
    Just (TyStr _) <- HashMap.lookup tyName schemaTypes,
    KeyPress [] KeyCode.Escape <- inputEvent
  = Just $ ActionExitEditMode selectionPath

  -- Delete character backward.
  | Just (SelectionTipLabeled tyName) <- selectionTip,
    Just (TyStr _) <- HashMap.lookup tyName schemaTypes,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.Backspace <- inputEvent
  = Just $ ActionDeleteCharBackward selectionPath

  -- Delete character forward.
  | Just (SelectionTipLabeled tyName) <- selectionTip,
    Just (TyStr _) <- HashMap.lookup tyName schemaTypes,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.Delete <- inputEvent
  = Just $ ActionDeleteCharForward selectionPath

  -- Move string cursor backward.
  | Just (SelectionTipLabeled tyName) <- selectionTip,
    Just (TyStr _) <- HashMap.lookup tyName schemaTypes,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.ArrowLeft <- inputEvent
  = Just $ ActionMoveStrCursorBackward selectionPath

  -- Move string cursor forward.
  | Just (SelectionTipLabeled tyName) <- selectionTip,
    Just (TyStr _) <- HashMap.lookup tyName schemaTypes,
    Just _ <- selectionStrPos,
    KeyPress [] KeyCode.ArrowRight <- inputEvent
  = Just $ ActionMoveStrCursorForward selectionPath

  | KeyPress [] keyCode <- inputEvent,
    keyLetter ',' keyCode
  = Just $ ActionAppendSeqItem selectionPath

  -- Insert letter.
  | Just (SelectionTipLabeled tyName) <- selectionTip,
    Just (TyStr _) <- HashMap.lookup tyName schemaTypes,
    Just _ <- selectionStrPos,
    KeyPress mods keyCode <- inputEvent,
    Control `notElem` mods,
    Just c <- keyChar keyCode
  = Just $ ActionInsertLetter selectionPath c

  -- Drop a node from the stack.
  | StackVisible <- stkVis,
    KeyPress [] keyCode <- inputEvent,
    keyLetter 'x' keyCode
  = Just ActionDropStack

  -- Enter jumptag mode.
  | Nothing <- selectionStrPos,
    KeyPress [] keyCode <- inputEvent,
    keyLetter 'g' keyCode
  = Just $ ActionActivateJumptags

  -- Delete node.
  | keyCodeLetter KeyCode.Delete 'x' inputEvent
  = Just $ ActionDeleteNode selectionPath

  -- Push a node to the stack.
  | KeyPress [] keyCode <- inputEvent,
    keyLetter 'y' keyCode
  = Just $ ActionPushStack selectionPath

  -- Pop/swap a node from the stack.
  | KeyPress [] keyCode <- inputEvent,
    keyLetter 'p' keyCode
  = Just $ ActionPopSwapStack selectionPath

  -- Rotate stack.
  | KeyPress [] keyCode <- inputEvent,
    keyLetter 'r' keyCode
  = Just ActionRotateStack

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

type ReactM s = WriterT UndoFlag (ReaderT ReactCtx (StateT s Maybe)) ()

applyAction :: Action -> ReactCtx -> ReactState -> Maybe (UndoFlag, ReactState)
applyAction act rctx rst =
  flip runStateT rst $
  flip runReaderT rctx $
  execWriterT $
  applyActionM act

applyActionM :: Action -> ReactM ReactState

applyActionM (ActionDeleteNode path) = do
  rstStackVis .= StackVisible
  nodes <-
    zoom (rstNode . atPath path) $ do
      node@Node{} <- get
      put Hole
      return [node]
  forM_ nodes $ \node ->
    rstStack %= (node:)
  setUndoFlag

applyActionM (ActionPushStack path) = do
  rstStackVis .= StackVisible
  parent <- use rstNode
  let nodes = parent ^.. atPath path
  forM_ nodes $ \node ->
    rstStack %= (node:)

applyActionM (ActionPopSwapStack path) = do
  rstStackVis .= StackVisible
  n:ns <- use rstStack
  rstStack .= ns
  popSwapNode path n

applyActionM ActionRotateStack = do
  stkVis <- use rstStackVis
  case stkVis of
    StackVisible -> rstStack %= rotate
    StackHidden -> rstStackVis .= StackVisible

applyActionM ActionDropStack = do
  rstStack %= List.drop 1

applyActionM (ActionExitEditMode path) =
  zoom (rstNode . atPath path) $ do
    Node nodeSel (ValueStr tyName str) <- get
    let NodeStrSel pos _ = nodeSel
    put $ Node (NodeStrSel pos False) (ValueStr tyName str)

applyActionM (ActionDeleteCharBackward path) =
  zoom (rstNode . atPath path) $ do
    Node nodeSel (ValueStr tyName str) <- get
    let NodeStrSel pos em = nodeSel
    guard (pos > 0)
    let pos' = pos - 1
    let (before, after) = Text.splitAt pos' str
        str' = before <> Text.drop 1 after
    put $ Node (NodeStrSel pos' em) (ValueStr tyName str')
    setUndoFlag

applyActionM (ActionDeleteCharForward path) =
  zoom (rstNode . atPath path) $ do
    Node nodeSel (ValueStr tyName str) <- get
    let NodeStrSel pos _ = nodeSel
    guard (pos < Text.length str)
    let (before, after) = Text.splitAt pos str
        str' = before <> Text.drop 1 after
    put $ Node nodeSel (ValueStr tyName str')
    setUndoFlag

applyActionM (ActionMoveStrCursorBackward path) =
  zoom (rstNode . atPath path) $ do
    Node nodeSel (ValueStr tyName str) <- get
    let NodeStrSel pos em = nodeSel
    guard (pos > 0)
    let pos' = pos - 1
    put $ Node (NodeStrSel pos' em) (ValueStr tyName str)

applyActionM (ActionMoveStrCursorForward path) =
  zoom (rstNode . atPath path) $ do
    Node nodeSel (ValueStr tyName str) <- get
    let NodeStrSel pos em = nodeSel
    guard (pos < Text.length str)
    let pos' = pos + 1
    put $ Node (NodeStrSel pos' em) (ValueStr tyName str)

applyActionM (ActionAppendSeqItem path) = do
  path' <- maybeA (pathParent path)
  zoom (rstNode . atPath path') $ do
    Node nodeSel (ValueSeq items) <- get
    let NodeSeqSel seqSel = nodeSel
    SeqSel i True <- pure seqSel
    let
      i' = indexToInt i + 1
      items' = Seq.insertAt i' Hole items
      seqSel' = SeqSel (intToIndex i') True
      nodeSel' = NodeSeqSel seqSel'
      value' = ValueSeq items'
    put $ Node nodeSel' value'

applyActionM (ActionInsertLetter path c) =
  zoom (rstNode . atPath path) $ do
    Node nodeSel (ValueStr tyName str) <- get
    let NodeStrSel pos editMode = nodeSel
    guard editMode
    let (before, after) = Text.splitAt pos str
        str' = before <> Text.singleton c <> after
        pos' = pos + 1
    put $ Node (NodeStrSel pos' editMode) (ValueStr tyName str')
    setUndoFlag

applyActionM (ActionSelectParent path) = do
  rstStackVis .= StackHidden
  path' <- maybeA (pathParent path)
  zoom (rstNode . atPath path') $ do
    Node nodeSel value <- get
    nodeSel' <- maybeA (toNodeSelSelf nodeSel)
    put $ Node nodeSel' value

applyActionM (ActionSelectChild path) = do
  rstStackVis .= StackHidden
  zoom (rstNode . atPath path) $ do
    Node nodeSel value <- get
    nodeSel' <- maybeA (toNodeSelChild nodeSel)
    put $ Node nodeSel' value

applyActionM (ActionSelectSiblingBackward path) = do
  rstStackVis .= StackHidden
  path' <- maybeA (pathParent path)
  zoom rstNode $ zoomPathPrefix path' $ do
    Node nodeSel value <- get
    nodeSel' <- case value of
      ValueRec tyName _ -> do
        let NodeRecSel recSel = nodeSel
        RecSel fieldName True <- pure recSel
        recMoveMaps <- view rctxRecMoveMaps
        let moveMap = rmmBackward (recMoveMaps HashMap.! tyName)
        fieldName' <- maybeA (HashMap.lookup fieldName moveMap)
        let recSel' = RecSel fieldName' True
        return (NodeRecSel recSel')
      ValueSeq _ -> do
        let NodeSeqSel seqSel = nodeSel
        SeqSel i True <- pure seqSel
        i' <- maybeA (indexPred i)
        let seqSel' = SeqSel i' True
        return (NodeSeqSel seqSel')
      _ -> A.empty
    put $ Node nodeSel' value

applyActionM (ActionSelectSiblingForward path) = do
  rstStackVis .= StackHidden
  path' <- maybeA (pathParent path)
  zoom rstNode $ zoomPathPrefix path' $ do
    Node nodeSel value <- get
    nodeSel' <- case value of
      ValueRec tyName _ -> do
        let NodeRecSel recSel = nodeSel
        RecSel fieldName True <- pure recSel
        recMoveMaps <- view rctxRecMoveMaps
        let moveMap = rmmForward (recMoveMaps HashMap.! tyName)
        fieldName' <- maybeA (HashMap.lookup fieldName moveMap)
        let recSel' = RecSel fieldName' True
        return (NodeRecSel recSel')
      ValueSeq items -> do
        let NodeSeqSel seqSel = nodeSel
        SeqSel i True <- pure seqSel
        i' <- maybeA (indexSucc items i)
        let seqSel' = SeqSel i' True
        return (NodeSeqSel seqSel')
      _ -> A.empty
    put $ Node nodeSel' value

applyActionM ActionActivateJumptags = do
  jumptags <- view rctxJumptags
  rstActiveJumptags .= List.zip jumptagLabels jumptags

applyActionM ActionDeactivateJumptags = do
  rstActiveJumptags .= []

applyActionM (ActionJumptagLookup c) = do
  activeJumptags <- use rstActiveJumptags
  let activeJumptags' =
        List.map (\(_, jt) -> jt) $
        List.filter (\(c', _) -> c == c') $
        activeJumptags
  case activeJumptags' of
    [] -> A.empty
    [Jumptag _ path] -> do
      rstNode %= setPathNode path
      rstActiveJumptags .= []
    jumptags -> rstActiveJumptags .= List.zip jumptagLabels jumptags

applyActionM ActionStartMotion = do
  rstMotion .= Just (Motion "")

applyActionM (ActionAppendMotion c) = do
  guard (not (Char.isSpace c))
  Just (Motion s) <- use rstMotion
  let motion' = Motion (s <> Text.singleton c)
  rstMotion .= Just motion'

applyActionM (ActionCommitMotion path) = do
  Just motion <- use rstMotion
  rstMotion .= Nothing
  alwaysSucceed $ case motion of
    Motion "" -> do
      -- Enter/Exit edit mode with Space.
      -- Use Shift-Space to enter a space character.
      zoom (rstNode . atPath path) $ do
        Node nodeSel value@(ValueStr _ _) <- get
        let NodeStrSel pos em = nodeSel
        put $ Node (NodeStrSel pos (not em)) value
    (insertSeqMotion -> Just n) ->
      popSwapNode path (defaultSeqNode n)
    _ -> do
      defaultNodes <- view rctxDefaultNodes
      let nodes = filterByMotion motion (HashMap.toList defaultNodes)
      [node] <- pure nodes
      popSwapNode path node

popSwapNode :: Path -> Node -> ReactM ReactState
popSwapNode path n = do
  nodes <-
    zoom (rstNode . atPath path) $ do
      node <- get
      put n
      setUndoFlag
      case node of
        Hole -> return []
        Node{} -> return [node]
  forM_ nodes $ \node -> do
    rstStackVis .= StackVisible
    rstStack %= (node:)

jumptagLabels :: [Char]
jumptagLabels = List.cycle "aoeuhtnspcrjm"
    -- Dvorak-friendly, should be configurable.

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

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
atPathSegment (PathSegmentSeq i) =
  \f node ->
    case node of
      Node nodeSel (ValueSeq items)
        | let i' = indexToInt i,
          Just a <- Seq.lookup i' items
        -> f a <&> \a' ->
             let items' = Seq.update i' a' items
             in Node nodeSel (ValueSeq items')
      _ -> pure node
atPathSegment (PathSegmentRec tyName fieldName) =
  \f node ->
    case node of
      Node nodeSel (ValueRec tyName' fields)
        | tyName == tyName',
          Just a <- HashMap.lookup fieldName fields
        -> f a <&> \a' ->
             let fields' = HashMap.insert fieldName a' fields
             in Node nodeSel (ValueRec tyName' fields')
      _ -> pure node

zoomPathPrefix :: Path -> ReactM Node -> ReactM Node
zoomPathPrefix p m =
  case unconsPath p of
    Nothing -> m
    Just (ps, p') ->
      zoom (atPathSegment ps) (zoomPathPrefix p' m) <|> m

matchMotion :: Motion -> TyName -> Bool
matchMotion (Motion motionStr) (TyName tyName) =
  let
    tyNameParts =
      List.map (Text.pack . List.map letterToChar . NonEmpty.toList) $
      NonEmpty.toList (nameParts tyName)
    motionParts = Text.splitOn "-" motionStr
    matchPart (motionPart, tyNamePart) =
      not (Text.null motionPart) &&
      (Text.isPrefixOf `on` Text.toCaseFold) motionPart tyNamePart
  in
    List.all matchPart (List.zip motionParts tyNameParts)

filterByMotion :: Motion -> [(TyName, a)] -> [a]
filterByMotion motion =
  List.map snd . List.filter (matchMotion motion . fst)

insertSeqMotion :: Motion -> Maybe Int
insertSeqMotion (Motion s)
  | Text.null s = Nothing
  | Text.all (==',') s = Just (Text.length s)
  | otherwise = Nothing

defaultSeqNode :: Int -> Node
defaultSeqNode n =
  Node (NodeSeqSel (SeqSel (intToIndex 0) True))
    (ValueSeq (Seq.replicate (n+1) Hole))

indexPred :: Index -> Maybe Index
indexPred i =
  let i' = indexToInt i in
  if i' > 0 then Just (intToIndex (i' - 1)) else Nothing

indexSucc :: Seq a -> Index -> Maybe Index
indexSucc xs i =
  let i' = indexToInt i + 1 in
  if i' < Seq.length xs then Just (intToIndex i') else Nothing

mkDefaultNodes :: Schema -> HashMap TyName RecMoveMap -> HashMap TyName Node
mkDefaultNodes schema recMoveMaps =
  HashMap.mapWithKey mkDefNode (schemaTypes schema)
  where
    mkDefNode :: TyName -> Ty -> Node
    mkDefNode tyName = \case
      TyStr _ -> Node (NodeStrSel 0 True) (ValueStr tyName "")
      TyRec fieldTys ->
        let
          fields = HashMap.map (const Hole) fieldTys
          recMoveMap = recMoveMaps HashMap.! tyName
          recSel =
            case rmmFieldOrder recMoveMap of
              [] -> RecSel0
              fieldName:_ -> RecSel fieldName True
        in
          Node (NodeRecSel recSel) (ValueRec tyName fields)

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
  jumptag = id

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
    { _pluginSchema :: Schema,
      _pluginRecLayouts :: HashMap TyName ALayoutFn
    }

-- | A plugin as consumed by the editor, with additional information
-- derived from the user specification.
data PluginInfo =
  PluginInfo
    { pluginInfoSchema :: Schema,
      pluginInfoRecLayouts :: HashMap TyName ALayoutFn,
      pluginInfoRecMoveMaps :: HashMap TyName RecMoveMap,
      pluginInfoDefaultNodes :: HashMap TyName Node
    }

makeLenses ''Plugin

mkPluginInfo :: Plugin -> PluginInfo
mkPluginInfo plugin =
  PluginInfo
    { pluginInfoSchema = schema,
      pluginInfoRecLayouts = recLayouts,
      pluginInfoRecMoveMaps = recMoveMaps,
      pluginInfoDefaultNodes = defaultNodes
    }
  where
    schema = plugin ^. pluginSchema
    recLayouts = plugin ^. pluginRecLayouts
    recMoveMaps = mkRecMoveMaps recLayouts
    defaultNodes = mkDefaultNodes schema recMoveMaps


--------------------------------------------------------------------------------
---- Parsing
--------------------------------------------------------------------------------

fromParsedValue :: PluginInfo -> ParsedValue -> Node
fromParsedValue pluginInfo = go
  where
    go (ParsedValue value) =
      Node (mkNodeSel value) (fmap go value)
    mkNodeSel (ValueStr _ str) =
      NodeStrSel (Text.length str) False
    mkNodeSel (ValueSeq items) =
      NodeSeqSel $
      if Seq.null items
      then SeqSel0
      else SeqSel (intToIndex 0) False
    mkNodeSel (ValueRec tyName _) =
      NodeRecSel $
      case rmmFieldOrder (recMoveMaps HashMap.! tyName) of
        [] -> RecSel0
        fieldName:_ -> RecSel fieldName False
    recMoveMaps = pluginInfoRecMoveMaps pluginInfo
