{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Source.NewGen
  ( -- * Names
    SynShape,
    Index,

    -- * Types
    Schema (..),
    TyUnion,

    -- * Values
    Node (..),
    RecSel (..),

    -- * Path
    PathSegment (..),
    Path (..),
    emptyPath,
    PathBuilder,

    -- * Draw
    offsetZero,
    CursorBlink (..),
    blink,
    Selection (..),
    Paths (..),
    Ann,
    El,
    redrawUI,
    cairoPositionedElementRender,
    Find (..),
    PrecPredicate,
    precAllow,
    precAllowAll,
    noPrec,
    WritingDirection (..),
    LayoutCtx (..),

    -- * Draw (ext)
    layoutNodeStandalone,
    foldCairoCollage,
    getNoAnn,
    defaultDrawCtx,
    cairoRender,

    -- * React
    ReactResult (..),

    -- * Editor
    Mode (..),
    quitStackMode,
    EditorState (..),
    initEditorState,
    fromParsedValue,
    esExpr,
    esPointer,
    esPrecBordersAlways,
    esWritingDirection,
    esStack,
    esUndo,
    esRedo,
    esRenderUI,
    esPointerPath,
    esJumptags,
    esMode,
    selectionOfEditorState,
    reactEditorState,

    -- * Plugin
    Plugin (..),
    PluginInfo,
    mkPluginInfo,

    -- * Utils
    inj,
    nothing,
    maybeA,
  )
where

import Control.Applicative as A
import Control.Lens as Lens hiding (Index, elements)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Void
import qualified Data.Char as Char
import Data.DList as DList
import Data.Foldable as Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Maybe
import Data.Primitive.Array as Array
import Data.Semigroup
import Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Rendering.Cairo as Cairo (Render)
import Inj
import Inj.Base ()
import Numeric.Natural (Natural)
import Numeric.NonNegative
import Sdam.Core
import Sdam.Parser
import Sdam.Validator
import Slay.Cairo.Element
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Combinators
import Slay.Core
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Source.Plugin
import Prelude hiding (seq)

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data Node = Node NodeSel (Syn Node)

data NodeSel
  = SynSel RecSel
  | StrSel Int

data RecSel
  = -- for records without children (and empty sequences)
    RecSel0
  | -- for records with children (and non-empty sequences)
    RecSel Index SelStatus

-- The node can be in one of there states:
--

-- * Self-selection, collapsed.

-- * Self-selection, not collapsed.

-- * Child selection.
--
-- Note that a node with a child selected cannot be collapsed.
-- This way we can be certain that the selected node is visible.

data SelStatus = SelSelf Collapsed | SelChild

newtype Collapsed = Collapsed Bool

--------------------------------------------------------------------------------
---- Validation
--------------------------------------------------------------------------------

toValidationValue :: Node -> ValidationValue
toValidationValue (Node _ syn) = ValidationValue (fmap toValidationValue syn)

validateNode :: Schema -> Node -> ValidationResult
validateNode schema node = validate schema (toValidationValue node)

--------------------------------------------------------------------------------
---- Drawing
--------------------------------------------------------------------------------

jumptagLabels :: NonEmpty Char
jumptagLabels =
  -- Dvorak-friendly, should be configurable.
  NonEmpty.fromList "aoeuhtnspcrjm"

data CursorBlink = CursorVisible | CursorInvisible

blink :: CursorBlink -> CursorBlink
blink = \case
  CursorVisible -> CursorInvisible
  CursorInvisible -> CursorVisible

data Selection
  = Selection
      { selectionPath :: Path,
        selectionTip :: SynShape,
        selectionTipPos :: Maybe Int
      }

data Paths
  = Paths
      { pathsCursor :: Maybe Path,
        pathsSelection :: Selection
      }

data DrawCtx
  = DrawCtx Selection CursorBlink
  | NoDrawCtx

defaultDrawCtx :: DrawCtx
defaultDrawCtx = NoDrawCtx

textline ::
  Inj (CairoElement ((->) DrawCtx)) a =>
  Color ->
  Font ->
  Text ->
  (DrawCtx -> Maybe Natural) ->
  a
textline color font str cur = text font (inj color) str cur

line :: Color -> Natural -> Collage Ann El
line color w = rect nothing (inj color) (Extents w 1)

vline :: Color -> Natural -> Collage Ann El
vline color h = rect nothing (inj color) (Extents 1 h)

leftOf :: Collage n El -> Integer
leftOf collage = toInteger (marginLeft (collageMargin collage))

rightOf :: Extents -> Collage n El -> Integer
rightOf (Extents vacantWidth _) collage =
  let Extents width _ = collageExtents collage
      mRight = marginRight (collageMargin collage)
      minus a b = max 0 (toInteger a - toInteger b)
   in vacantWidth `minus` (width + mRight)

data WritingDirection = WritingDirectionLTR | WritingDirectionRTL

newtype TokenCount = TokenCount Natural

addTokenCount :: Int -> TokenCount -> TokenCount
addTokenCount n (TokenCount k) = TokenCount (fromIntegral n + k)

newtype RecLayoutFn
  = RecLayoutFn
      { appRecLayoutFn ::
          Path ->
          TokenCount ->
          WritingDirection ->
          (TokenCount, (PrecUnenclosed, Collage Ann El))
      }

instance Semigroup RecLayoutFn where
  RecLayoutFn a <> RecLayoutFn b =
    RecLayoutFn $ \path tc wd ->
      let (tc', (aUnenclosed, a')) =
            a path tc wd
          (tc'', (bUnenclosed, b')) =
            b path tc' wd
          f = case wd of
            WritingDirectionLTR -> horizBaseline
            WritingDirectionRTL -> flip horizBaseline
       in (tc'', (aUnenclosed <> bUnenclosed, f a' b'))

nefoldr1 :: (a -> a -> a) -> NonEmpty a -> a
nefoldr1 = List.foldr1 -- safe for non-empty lists

data RecLayoutStyle = RecLayoutStyle {recLayoutEditMode :: Bool}

data PunctList a
  = PunctBase Text
  | PunctCons Text a (PunctList a)

splitEither :: (a -> Either sep b) -> [a] -> ([b], Maybe (sep, [a]))
splitEither _ [] = ([], Nothing)
splitEither p (a : as) =
  case p a of
    Left sep -> ([], Just (sep, as))
    Right b -> mapFst (b :) (splitEither p as)

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

matchTokenNode :: Token a -> Either a Char
matchTokenNode (TokenChar c) = Right c
matchTokenNode (TokenNode a) = Left a

parseSynPunctList :: [Token a] -> PunctList a
parseSynPunctList ts =
  case splitEither matchTokenNode ts of
    (s, Nothing) -> PunctBase (Text.pack s)
    (s, Just (node, ts')) -> PunctCons (Text.pack s) node (parseSynPunctList ts')

synRows :: PunctList a -> NonEmpty (PunctList a)
synRows (PunctBase "/") = PunctBase "" :| [PunctBase ""]
synRows (PunctBase p) = PunctBase p :| []
synRows (PunctCons "/" a ps) =
  let p' :| ps' = synRows ps
   in PunctBase "" :| PunctCons "" a p' : ps'
synRows (PunctCons p a ps) =
  let p' :| ps' = synRows ps
   in PunctCons p a p' :| ps'

synToLayout :: RecLayoutStyle -> Syn (PrecUnenclosed, Collage Ann El) -> RecLayoutFn
synToLayout rs syn =
  nefoldr1 vsep $ fmap (shapeRowToLayout rs) rows
  where
    rows = synRows (parseSynPunctList (Foldable.toList (synTokens syn)))
    RecLayoutFn a `vsep` RecLayoutFn b =
      RecLayoutFn $ \path tc wd ->
        let (tc', (aUnenclosed, a')) =
              a path tc wd
            (tc'', (bUnenclosed, b')) =
              b path (addTokenCount 1 tc') wd
            f = case wd of
              WritingDirectionLTR -> vertLeft
              WritingDirectionRTL -> vertRight
            maxWidth = (max `on` widthOf) a' b'
         in (tc'', (aUnenclosed <> bUnenclosed, a' `f` line light1 maxWidth `f` b'))

shapeRowToLayout :: RecLayoutStyle -> PunctList (PrecUnenclosed, Collage Ann El) -> RecLayoutFn
shapeRowToLayout rs row =
  case nonEmpty (shapeRowToLayouts rs row) of
    Nothing -> punctToLayout ""
    Just row' -> sconcat row'

shapeRowToLayouts :: RecLayoutStyle -> PunctList (PrecUnenclosed, Collage Ann El) -> [RecLayoutFn]
shapeRowToLayouts rs (PunctBase p) = punctToLayouts rs p
shapeRowToLayouts rs (PunctCons p fld l) =
  punctToLayouts rs p <> [fieldToLayout fld] <> shapeRowToLayouts rs l

fieldToLayout :: (PrecUnenclosed, Collage Ann El) -> RecLayoutFn
fieldToLayout fld = RecLayoutFn $ \_ tc _ -> (addTokenCount 1 tc, fld)

punctToLayouts :: RecLayoutStyle -> Text -> [RecLayoutFn]
punctToLayouts rs "" | not (recLayoutEditMode rs) = []
punctToLayouts _ p = [punctToLayout p]

punctToLayout :: Text -> RecLayoutFn
punctToLayout s =
  RecLayoutFn $ \path tc _ ->
    let tc' = addTokenCount (Text.length s) tc
        a =
          textline
            light1
            ubuntuFont
            s
            (\drawCtx -> blinkingCursorPos path drawCtx >>= adjustByTokenCount tc)
        a' = if jumptagFits then layoutWithJumptag path a else a
        jumptagFits = widthOf a >= jumptagLabelMaxWidth
     in (tc', (mempty, a'))
  where
    adjustByTokenCount (TokenCount k) pos
      | pos >= k,
        let pos' = pos - k,
        pos' <= fromIntegral (Text.length s) =
        Just pos'
    adjustByTokenCount _ _ = Nothing

blinkingCursorPos :: Path -> DrawCtx -> Maybe Natural
blinkingCursorPos path (DrawCtx selection CursorVisible)
  | selectionPath selection == path,
    Just pos <- selectionTipPos selection =
    Just (fromIntegral pos)
blinkingCursorPos _ _ = Nothing

layoutWithJumptag :: Path -> Collage Ann El -> Collage Ann El
layoutWithJumptag path =
  collageAnnotate (\o -> (mempty, mempty, pathJumptag o))
  where
    pathJumptag offset = DList.singleton (Jumptag offset path)

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
type El = CairoElement ((->) DrawCtx)

-- Collage annotations:
type Ann = (FindPath, FindZone, DList Jumptag)

getNoAnn :: ((), a) -> a
getNoAnn = snd

renderStackNodes :: Offset -> Collage () (CairoElement g) -> Paths -> FindZone -> CairoRender g
renderStackNodes fallbackOffset stackLayout Paths {pathsSelection} (Find findZone) =
  getNoAnn $ foldCairoCollage offset stackLayout
  where
    offset =
      case findZone (selectionPath pathsSelection) of
        Just (o, e) ->
          Offset
            { offsetX = offsetX o + toInteger (marginLeft (collageMargin stackLayout) + extentsW e),
              offsetY = offsetY o
            }
        Nothing -> fallbackOffset

renderSelectionBorder :: Paths -> FindZone -> CairoRender Identity
renderSelectionBorder Paths {pathsSelection} (Find findZone) =
  case findZone (selectionPath pathsSelection) of
    Just (o, e) ->
      getNoAnn
        $ foldCairoCollage o
        $ outline 2 color e
    Nothing -> mempty
  where
    color =
      case selectionTipPos pathsSelection of
        Nothing -> selectionBorderColor
        Just _ -> inputBorderColor

renderHoverBorder :: Paths -> FindZone -> CairoRender Identity
renderHoverBorder Paths {pathsCursor} (Find findZone) =
  case pathsCursor of
    Just p
      | Just (o, e) <- findZone p ->
        getNoAnn
          $ foldCairoCollage o
          $ outline 2 hoverBorderColor e
    _ -> mempty

renderJumptagLabels :: NonEmpty (Char, Jumptag) -> CairoRender Identity
renderJumptagLabels = foldMap renderJumptagLabel

renderJumptagLabel :: (Char, Jumptag) -> CairoRender Identity
renderJumptagLabel (c, Jumptag o _) =
  getNoAnn $ foldCairoCollage o $ layoutJumptagLabel c

layoutJumptagLabel :: Monoid n => Char -> Collage n (CairoElement Identity)
layoutJumptagLabel c =
  substrate 1 (rect nothing dark2) $
    text ubuntuMonoFont (rgb 255 127 80) label nothing
  where
    label = Text.toUpper (Text.singleton c)

jumptagLabelMaxWidth :: Natural
jumptagLabelMaxWidth =
  maximum @NonEmpty $
    fmap (widthOf . layoutJumptagLabel @()) jumptagLabels

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
dark1 = grayscale 41
dark1' = grayscale 35
dark2 = grayscale 77
light1 = grayscale 179
white = grayscale 255
red = rgb 255 0 0

selectionBorderColor,
  hoverBorderColor,
  stackBorderColor,
  inputBorderColor ::
    Inj Color a => a
selectionBorderColor = rgb 94 80 134
hoverBorderColor = rgb 255 127 80
stackBorderColor = rgb 45 134 108
inputBorderColor = rgb 45 134 108

textWithCursor :: Text -> (DrawCtx -> Maybe Natural) -> Collage Ann El
textWithCursor = textline white ubuntuFont

textWithoutCursor :: Text -> Collage Ann El
textWithoutCursor t = textWithCursor t nothing

class Inj t (f t) => Inj1 f t

instance Inj t (f t) => Inj1 f t

outline ::
  Inj (CairoElement f) a =>
  Inj1 f (Maybe (LRTB (NonNegative Double))) =>
  Natural ->
  f (Maybe Color) ->
  Extents ->
  a
outline width = rect (inj (Just outlineWidth))
  where
    outlineWidth :: LRTB (NonNegative Double)
    outlineWidth = pure (fromIntegral width)

punct ::
  Inj (CairoElement ((->) DrawCtx)) a =>
  Text ->
  a
punct t = textline light1 ubuntuFont t nothing

ubuntuFont :: Font
ubuntuFont = Font "Ubuntu" 12 FontWeightNormal

ubuntuMonoFont :: Font
ubuntuMonoFont = Font "Ubuntu Mono" 12 FontWeightNormal

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

nothing :: Inj (Maybe Void) a => a
nothing = inj (Nothing @Void)

-- These overlapping instances should make it upstream in a better
-- form (no overlapping).

instance {-# OVERLAPPING #-} Inj Void Natural where
  inj = absurd

instance {-# OVERLAPPING #-} Inj Void (LRTB a) where
  inj = absurd

maybeA :: Alternative f => Maybe a -> f a
maybeA = maybe A.empty A.pure

alwaysSucceed :: Alternative f => f () -> f ()
alwaysSucceed f = f <|> pure ()

--------------------------------------------------------------------------------
---- Input Mode
--------------------------------------------------------------------------------

newtype InputTrie = InputTrie (Map Char (Either InputTrie Char))

instance Semigroup InputTrie where
  InputTrie m1 <> InputTrie m2 =
    InputTrie $ Map.unionWith f m1 m2
    where
      f (Right c) _ = Right c
      f _ (Right c) = Right c
      f (Left t1) (Left t2) = Left (t1 <> t2)

instance Monoid InputTrie where
  mempty = InputTrie Map.empty

buildInputTrie :: [(String, Char)] -> InputTrie
buildInputTrie = foldMap pairToInputTrie
  where
    pairToInputTrie (s, c) = go c s
    go _ [] = error "buildInputTrie: bad input"
    go c [k] = InputTrie (Map.singleton k (Right c))
    go c (k : ks) = InputTrie (Map.singleton k (Left (go c ks)))

initialInputTrie :: InputTrie
initialInputTrie =
  buildInputTrie
    [ ("_", '_'),
      ("\\", '\\'),
      ("lam", 'λ'),
      ("forall", '∀'),
      ("Pi", 'Π'),
      ("star", '★'),
      ("all", '∗'),
      ("box", '□'),
      ("->", '→'),
      ("<-", '←'),
      ("not", '¬')
    ]

--------------------------------------------------------------------------------
---- Editor
--------------------------------------------------------------------------------

defaultHole :: Node
defaultHole = Node (SynSel RecSel0) (Syn Seq.empty)

isHole :: Node -> Bool
isHole (Node _ syn) = Seq.null (synTokens syn)

data JumpAction
  = JumpSelect
  | JumpCopyTo Path

data Mode
  = ModeNormal
  | ModeStack
  | ModeJump (NonEmpty (Char, Jumptag)) JumpAction
  | ModeInput Text InputTrie
  | ModeStackInput

quitStackMode :: Mode -> Mode
quitStackMode ModeStack = ModeNormal
quitStackMode mode = mode

data EditorState
  = EditorState
      { _esExpr :: Node,
        _esPointer :: Offset,
        _esPrecBordersAlways :: Bool,
        _esWritingDirection :: WritingDirection,
        _esStack :: [Node],
        _esUndo :: [Node],
        _esRedo :: [Node],
        _esRenderUI :: CursorBlink -> Cairo.Render (),
        _esPointerPath :: Maybe Path,
        _esJumptags :: [Jumptag],
        _esMode :: Mode
      }

initEditorState :: EditorState
initEditorState =
  EditorState
    { _esExpr = defaultHole,
      _esPointer = offsetZero,
      _esPrecBordersAlways = False,
      _esWritingDirection = WritingDirectionLTR,
      _esStack = [],
      _esUndo = [],
      _esRedo = [],
      _esRenderUI = const (pure ()),
      _esPointerPath = Nothing,
      _esJumptags = [],
      _esMode = ModeNormal
    }

data LayoutCtx
  = LayoutCtx
      { _lctxPath :: PathBuilder,
        _lctxValidationResult :: ValidationResult,
        _lctxViewport :: Extents,
        _lctxPrecBordersAlways :: Bool,
        _lctxPrecInfo :: HashMap SynShape (Array PrecPredicate),
        _lctxShapeNames :: HashMap SynShape ShapeName,
        _lctxPlaceholder :: Maybe Text,
        _lctxPrecPredicate :: PrecPredicate,
        _lctxWritingDirection :: WritingDirection
      }

data ReactCtx
  = ReactCtx
      { _rctxJumptags :: [Jumptag]
      }

data ReactState
  = ReactState
      { _rstNode :: Node,
        _rstStack :: [Node],
        _rstMode :: Mode
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
  es
    & esRenderUI .~ renderUI
    & esPointerPath .~ cursor
    & esJumptags .~ jumptags
  where
    lctx =
      LayoutCtx
        { _lctxPath = mempty @PathBuilder,
          _lctxValidationResult = mempty,
          _lctxViewport = viewport,
          _lctxPrecBordersAlways = es ^. esPrecBordersAlways,
          _lctxPrecInfo = pluginInfoPrecInfo pluginInfo,
          _lctxShapeNames = pluginInfoShapeNames pluginInfo,
          _lctxPlaceholder = Nothing,
          _lctxPrecPredicate = precAllowAll,
          _lctxWritingDirection = es ^. esWritingDirection
        }
    schema = pluginInfoSchema pluginInfo
    pointer = es ^. esPointer
    infoBarLayout = layoutInfoBar lctx es
    stackLayout = layoutNodesStack lctx (es ^. esStack)
    mainLayout = layoutMainExpr schema lctx (es ^. esExpr)
    hOff :: Collage n a -> Integer
    hOff c =
      toInteger (heightOf infoBarLayout)
        + toInteger (marginTop (collageMargin c))
        + 10
    mainOffset =
      Offset
        { offsetX = leftOf mainLayout + 10,
          offsetY = hOff mainLayout
        }
    stackFallbackOffset =
      Offset
        { offsetX = rightOf (lctx ^. lctxViewport) stackLayout,
          offsetY = hOff stackLayout
        }
    backgroundRdr =
      getNoAnn
        $ foldCairoCollage offsetZero
        $ rect nothing dark1 (lctx ^. lctxViewport)
    ((Find findPath, findZone, jumptags'), mainRdr) =
      foldCairoCollage mainOffset mainLayout
    infoBarRdr = getNoAnn $ foldCairoCollage offsetZero infoBarLayout
    jumptags = DList.toList jumptags'
    stackNodesVisible =
      case es ^. esMode of
        ModeStack -> True
        ModeStackInput -> True
        _ -> False
    stackRdr =
      if stackNodesVisible
        then renderStackNodes stackFallbackOffset stackLayout paths findZone
        else mempty
    jumptagsRdr = case es ^. esMode of
      ModeJump activeJumptags _ -> renderJumptagLabels activeJumptags
      _ -> mempty
    cursor = findPath pointer
    selection = selectionOfEditorState es
    paths = Paths {pathsCursor = cursor, pathsSelection = selection}
    renderUI cursorBlink = do
      cairoRender backgroundRdr runIdentity
      cairoRender mainRdr ($ DrawCtx selection cursorBlink)
      cairoRender (renderSelectionBorder paths findZone) runIdentity
      cairoRender (renderHoverBorder paths findZone) runIdentity
      cairoRender jumptagsRdr runIdentity
      cairoRender stackRdr ($ defaultDrawCtx)
      cairoRender infoBarRdr ($ DrawCtx selection cursorBlink)

layoutMainExpr ::
  Schema ->
  LayoutCtx ->
  Node ->
  Collage Ann El
layoutMainExpr schema lctx expr = collage
  where
    lctx' n = lctx {_lctxValidationResult = validateNode schema n}
    (_, collage) = layoutNode (lctx' expr) expr

layoutNodesStack :: Monoid n => LayoutCtx -> [Node] -> Collage n El
layoutNodesStack lctx nodes =
  case nonEmpty nodes of
    Nothing -> layoutStackDecoration $ punct "end of stack"
    Just ns -> nefoldr1 vertLeft (NonEmpty.map (layoutNodeStack lctx) ns)

layoutNodeStack :: Monoid n => LayoutCtx -> Node -> Collage n El
layoutNodeStack lctx node =
  mapCollageAnnotation (const mempty)
    $ layoutStackDecoration
    $ snd (layoutNode lctx' node)
  where
    lctx' = lctx {_lctxValidationResult = mempty}

layoutStackDecoration :: Monoid n => Collage n El -> Collage n El
layoutStackDecoration =
  collageWithMargin (Margin 4 4 4 4)
    . substrate 0 backgroundRect
    . substrate 4 (outline 2 stackBorderColor)
  where
    backgroundRect = rect nothing dark1'

layoutNodeStandalone :: Monoid n => LayoutCtx -> Node -> Collage n El
layoutNodeStandalone lctx node =
  mapCollageAnnotation (const mempty)
    $ substrate 0 backgroundRect
    $ substrate 4 (outline 2 stackBorderColor)
    $ snd (layoutNode lctx' node)
  where
    lctx' = lctx {_lctxValidationResult = mempty}
    backgroundRect = rect nothing dark1'

layoutInfoBar ::
  Monoid n =>
  LayoutCtx ->
  EditorState ->
  Collage n El
layoutInfoBar lctx es =
  case es ^. esMode of
    ModeInput acc (InputTrie t) ->
      mapCollageAnnotation (const mempty)
        $ substrate 0 (rect nothing inputBorderColor . extentsMax e)
        $ textWithoutCursor ("\\" <> acc <> "[" <> Text.pack (Map.keys t) <> "]")
    _ ->
      mapCollageAnnotation (const mempty)
        $ substrate 0 (rect nothing selectionBorderColor . extentsMax e)
        $ textWithoutCursor (pprSelection (lctx ^. lctxShapeNames) (selectionOfEditorState es))
  where
    e = Extents
      { extentsW = extentsW (lctx ^. lctxViewport),
        extentsH = 15
      }

pprSelection :: HashMap SynShape ShapeName -> Selection -> Text
pprSelection shapeNames selection = Text.pack (goPath selectionPath "")
  where
    Selection {selectionPath, selectionTip, selectionTipPos} = selection
    goPath p =
      case unconsPath p of
        Nothing -> goTip selectionTip . goTipPos selectionTipPos
        Just (ps, p') -> goPathSegment ps . (" → " ++) . goPath p'
    goTip shape =
      case HashMap.lookup shape shapeNames of
        Nothing -> (pprShape shape ++)
        Just a -> (Text.unpack (shapeName a) ++)
    goTipPos Nothing = id
    goTipPos (Just pos) = (" [" ++) . shows pos . (']' :)
    goPathSegment (PathSegment shape i) =
      case HashMap.lookup shape shapeNames of
        Nothing -> (pprShape shape ++) . (" [" ++) . shows (indexToInt i) . (']' :)
        Just a -> (Text.unpack (pprNamedShape a i) ++)
    pprNamedShape a i =
      shapeName a
        <> " ["
        <> Array.indexArray (shapeFieldNames a) (indexToInt i)
        <> "]"
    pprShape = concatMap escape . flattenSynShape
    escape '\n' = "\\n"
    escape c = [c]

layoutNode :: LayoutCtx -> Node -> (PrecUnenclosed, Collage Ann El)
layoutNode lctx (Node (SynSel synSel) _)
  | isNodeCollapsed synSel = layoutCollapsed lctx
layoutNode lctx (Node sel syn)
  | Just lit <- to_lit lfields = layoutLit recLayoutStyle lctx lit
  | Just seq <- to_seq lfields = layoutSeq recLayoutStyle lctx seq
  | Just str <- to_str lfields = layoutStr lctx str
  | otherwise = layoutRec recLayoutStyle lctx lfields
  where
    lfields = layoutFields recLayoutStyle lctx syn
    recLayoutStyle =
      case sel of
        StrSel _ -> RecLayoutStyle {recLayoutEditMode = True}
        SynSel _ -> RecLayoutStyle {recLayoutEditMode = False}

layoutFields ::
  RecLayoutStyle ->
  LayoutCtx ->
  Syn Node ->
  Syn (PrecUnenclosed, Collage Ann El)
layoutFields rs lctx syn =
  syn & traversed %@~ \(intToIndex -> i) ->
    layoutNode
      $ lctxResetPrecEditMode
      $ lctxDescent (PathSegment shape i)
      $ lctx
  where
    shape = synShape syn
    lctxResetPrecEditMode
      | recLayoutEditMode rs = lctxPrecPredicate .~ noPrec
      | otherwise = id

layoutCollapsed :: LayoutCtx -> (PrecUnenclosed, Collage Ann El)
layoutCollapsed lctx =
  (,) (mempty @PrecUnenclosed)
    $ layoutSel (BorderValid precBorder) path
    $ layoutWithJumptag path
    $ punct "…"
  where
    precBorder = PrecBorder (lctx ^. lctxPrecBordersAlways)
    path = buildPath (lctx ^. lctxPath)

layoutLit ::
  RecLayoutStyle ->
  LayoutCtx ->
  Seq (Token (PrecUnenclosed, Collage Ann El)) ->
  (PrecUnenclosed, Collage Ann El)
layoutLit rs lctx lit =
  (,) (guardUnenclosed precBorder precUnenclosed)
    $ layoutSel (toBorder lctx precBorder) path
    $ collage
  where
    (precUnenclosed, collage) =
      snd $ appRecLayoutFn layoutFn path (TokenCount 0) wd
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways)
        <> appPrecPredicate (lctx ^. lctxPrecPredicate) precUnenclosed
    layoutFn =
      punctToLayout "“" <> shapeRowToLayout rs pl <> punctToLayout "”"
      where
        pl = parseSynPunctList (Foldable.toList lit)
    path = buildPath (lctx ^. lctxPath)
    wd = lctx ^. lctxWritingDirection

layoutStr ::
  LayoutCtx ->
  Text ->
  (PrecUnenclosed, Collage Ann El)
layoutStr lctx str =
  (,) (mempty @PrecUnenclosed)
    $ layoutSel (toBorder lctx precBorder) path
    $ layoutWithJumptag path
    $ holeOverlay
    $ textWithCursor str (blinkingCursorPos path)
  where
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways)
        <> PrecBorder (Text.any Char.isSpace str)
    path = buildPath (lctx ^. lctxPath)
    hole = textline dark2 ubuntuFont ("_" <> fromMaybe "" (lctx ^. lctxPlaceholder)) nothing
    holeOverlay
      | Text.null str = collageCompose offsetZero hole
      | otherwise = id

lctxDescent :: PathSegment -> LayoutCtx -> LayoutCtx
lctxDescent pathSegment lctx =
  lctx
    & lctxPath %~ (<> mkPathBuilder pathSegment)
    & lctxValidationResult %~ pathTrieLookup pathSegment
    & lctxPlaceholder .~ placeholder
    & lctxPrecPredicate .~ precPredicate
  where
    PathSegment shape i = pathSegment
    placeholder =
      case HashMap.lookup shape (lctx ^. lctxShapeNames) of
        Nothing -> Nothing
        Just a -> Just (Array.indexArray (shapeFieldNames a) (indexToInt i))
    precPredicate
      | Just _ <- to_seq shape = precAllowAll
      | Just precInfo <- HashMap.lookup shape (lctx ^. lctxPrecInfo) =
        Array.indexArray precInfo (indexToInt i)
      | otherwise = noPrec

layoutRec ::
  RecLayoutStyle ->
  LayoutCtx ->
  Syn (PrecUnenclosed, Collage Ann El) ->
  (PrecUnenclosed, Collage Ann El)
layoutRec rs lctx syn =
  (,) (guardUnenclosed precBorder precUnenclosed')
    $ layoutSel (toBorder lctx precBorder) path
    $ collage
  where
    shape = synShape syn
    (precUnenclosed, collage) =
      snd $ appRecLayoutFn (synToLayout rs syn) path (TokenCount 0) wd
    precUnenclosed' = addUnenclosed shape precUnenclosed
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways)
        <> appPrecPredicate (lctx ^. lctxPrecPredicate) precUnenclosed'
    path = buildPath (lctx ^. lctxPath)
    wd = lctx ^. lctxWritingDirection

layoutSeq ::
  RecLayoutStyle ->
  LayoutCtx ->
  [(PrecUnenclosed, Collage Ann El)] ->
  (PrecUnenclosed, Collage Ann El)
layoutSeq rs lctx seq =
  (,) (guardUnenclosed precBorder precUnenclosed)
    $ layoutSel (toBorder lctx precBorder) path
    $ collage
  where
    (precUnenclosed, collage) =
      snd $ appRecLayoutFn layoutFn path (TokenCount 0) wd
    precBorder =
      PrecBorder (lctx ^. lctxPrecBordersAlways)
        <> appPrecPredicate (lctx ^. lctxPrecPredicate) precUnenclosed
    layoutFn =
      case nonEmpty seq of
        Nothing -> punctToLayout "|"
        Just seq' ->
          if recLayoutEditMode rs
            then punctToLayout "|" <> rowsLayout seq'
            else nefoldr1 vcat (fmap (addB . fieldToLayout) seq')
    rowsLayout (h :| hs) =
      case nonEmpty hs of
        Nothing -> fieldToLayout h <> emptyPunctLayout
        Just hs' -> vcat (fieldToLayout h <> emptyPunctLayout) (rowsLayout hs')
    emptyPunctLayout = punctToLayout ""
    path = buildPath (lctx ^. lctxPath)
    wd = lctx ^. lctxWritingDirection

addB :: RecLayoutFn -> RecLayoutFn
addB (RecLayoutFn x) =
  RecLayoutFn $ \path tc wd ->
    let (tc', (xUnenclosed, x')) =
          x path tc wd
        g = case wd of
          WritingDirectionLTR -> horizTop
          WritingDirectionRTL -> flip horizTop
     in (tc', (xUnenclosed, vline dark2 (heightOf x') `g` x'))

vcat :: RecLayoutFn -> RecLayoutFn -> RecLayoutFn
RecLayoutFn a `vcat` RecLayoutFn b =
  RecLayoutFn $ \path tc wd ->
    let (tc', (aUnenclosed, a')) =
          a path tc wd
        (tc'', (bUnenclosed, b')) =
          b path tc' wd
        f = case wd of
          WritingDirectionLTR -> vertLeft
          WritingDirectionRTL -> vertRight
     in (tc'', (aUnenclosed <> bUnenclosed, a' `f` b'))

to_lit :: Syn a -> Maybe (Seq (Token a))
to_lit (synTokens -> Seq.viewl -> TokenChar '"' Seq.:< ts) = Just ts
to_lit _ = Nothing

to_str :: Syn a -> Maybe Text
to_str syn
  | Foldable.null syn =
    Just (Text.pack [c | TokenChar c <- Foldable.toList (synTokens syn)])
to_str _ = Nothing

to_seq :: Syn a -> Maybe [a]
to_seq (synTokens -> Foldable.toList -> TokenChar '|' : init_ts) = to_seq' init_ts
  where
    to_seq' [] = Just []
    to_seq' (TokenChar _ : _) = Nothing
    to_seq' (TokenNode n : ts) = (n :) <$> to_seq' ts
to_seq _ = Nothing

data Border = BorderValid PrecBorder | BorderInvalid

layoutSel :: Border -> Path -> Collage Ann El -> Collage Ann El
layoutSel border path =
  collageWithMargin (mkMargin (marginWidth - borderWidth))
    . collageAnnotateMargin pathZone
    . layoutBorder borderWidth border
    . collageWithMargin (mkMargin marginWidth)
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
    allowUnknownShapes = HashSet.filter (not . isUnknownShapeError)
    isUnknownShapeError (UnknownShape _) = True
    isUnknownShapeError _ = False
    validChild =
      HashSet.null (allowUnknownShapes (pathTrieRoot (lctx ^. lctxValidationResult)))

--------------------------------------------------------------------------------
---- Editor - Selection
--------------------------------------------------------------------------------

selectionOfEditorState :: EditorState -> Selection
selectionOfEditorState es = selectionOfNode (es ^. esExpr)

selectionOfNode :: Node -> Selection
selectionOfNode = \case
  Node (StrSel pos) syn -> Selection emptyPath (synShape syn) (Just pos)
  Node (SynSel recSel) syn ->
    case recSel of
      RecSel0 -> Selection emptyPath (synShape syn) Nothing
      RecSel _ (SelSelf _) -> Selection emptyPath (synShape syn) Nothing
      RecSel i SelChild ->
        let pathSegment = PathSegment (synShape syn) i
            recField = syn ^?! synIx i
            Selection pathTail tip tipPos = selectionOfNode recField
         in Selection (consPath pathSegment pathTail) tip tipPos

-- | Set self-selection for all nodes.
resetPathNode :: Node -> Node
resetPathNode (Node nodeSel syn) =
  Node nodeSel' (fmap resetPathNode syn)
  where
    nodeSel' =
      case nodeSel of
        SynSel sel -> SynSel (toRecSelSelf sel)
        StrSel _ -> doneEditing syn

updatePathNode :: Path -> Node -> Node
updatePathNode path node = case node of
  Node (StrSel _) _ -> node
  Node (SynSel sel) syn ->
    case unconsPath path of
      Nothing -> Node (SynSel (toRecSelSelf sel)) syn
      Just (PathSegment shape i, path') ->
        if shape /= synShape syn || hasn't (synIx i) syn
          then node
          else
            Node
              (SynSel (RecSel i SelChild))
              (over (synIx i) (updatePathNode path') syn)

setPathNode :: Path -> Node -> Node
setPathNode path node = updatePathNode path (resetPathNode node)

toSelSelf :: SelStatus -> SelStatus
toSelSelf SelChild = SelSelf (Collapsed False)
toSelSelf selStatus@(SelSelf _collapsed) = selStatus

toRecSelSelf :: RecSel -> RecSel
toRecSelSelf RecSel0 = RecSel0
toRecSelSelf (RecSel i selStatus) = RecSel i (toSelSelf selStatus)

toRecSelChild :: RecSel -> Maybe RecSel
toRecSelChild RecSel0 = Nothing
toRecSelChild (RecSel i _) = Just (RecSel i SelChild)

toggleNodeCollapse :: RecSel -> Maybe RecSel
toggleNodeCollapse (RecSel i selStatus) =
  toggleSelStatusCollapse selStatus <&> \selStatus' ->
    RecSel i selStatus'
toggleNodeCollapse _ = Nothing

toggleSelStatusCollapse :: SelStatus -> Maybe SelStatus
toggleSelStatusCollapse SelChild = Nothing
toggleSelStatusCollapse (SelSelf collapsed) =
  Just (SelSelf (toggleCollapsed collapsed))

toggleCollapsed :: Collapsed -> Collapsed
toggleCollapsed (Collapsed c) = Collapsed (not c)

isNodeCollapsed :: RecSel -> Bool
isNodeCollapsed (RecSel _ selStatus) = isSelStatusCollapsed selStatus
isNodeCollapsed _ = False

isSelStatusCollapsed :: SelStatus -> Bool
isSelStatusCollapsed SelChild = False
isSelStatusCollapsed (SelSelf (Collapsed c)) = c

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

reactEditorState ::
  PluginInfo ->
  InputEvent ->
  EditorState ->
  ReactResult EditorState
reactEditorState _ (PointerMotion x y) es =
  ReactOk $
    es & esPointer .~ Offset (fromIntegral x) (fromIntegral y)
reactEditorState pluginInfo ButtonPress es
  | Just p <- es ^. esPointerPath,
    ModeJump _ jumpAction <- es ^. esMode,
    let act = commitJumpAction p jumpAction,
    Just es' <- runReactM_EditorState pluginInfo act es =
    ReactOk es'
reactEditorState _ ButtonPress es
  | Just p <- es ^. esPointerPath =
    ReactOk $
      es
        & esExpr %~ setPathNode p
        & esMode .~ ModeNormal
reactEditorState _ (KeyPress [Control] keyCode) es
  | keyLetter 'b' keyCode =
    ReactOk $ es & esPrecBordersAlways %~ not
  | keyLetter 'w' keyCode =
    ReactOk $
      es & esWritingDirection %~ \case
        WritingDirectionLTR -> WritingDirectionRTL
        WritingDirectionRTL -> WritingDirectionLTR
  | keyLetter 'z' keyCode,
    (u : us) <- es ^. esUndo,
    let expr = es ^. esExpr =
    ReactOk $
      es
        & esExpr .~ u
        & esUndo .~ us
        & esRedo %~ (expr :)
  | keyLetter 'r' keyCode,
    (r : rs) <- es ^. esRedo,
    let expr = es ^. esExpr =
    ReactOk $
      es
        & esExpr .~ r
        & esRedo .~ rs
        & esUndo %~ (expr :)
reactEditorState pluginInfo inputEvent es
  | Just act <-
      getAction
        (es ^. esWritingDirection)
        (selectionOfNode (es ^. esExpr))
        (es ^. esMode)
        inputEvent,
    Just es' <- runReactM_EditorState pluginInfo (applyActionM act) es =
    ReactOk es'
reactEditorState _ _ _ = UnknownEvent

runReactM_EditorState ::
  PluginInfo ->
  ReactM ReactState ->
  EditorState ->
  Maybe EditorState
runReactM_EditorState _ act editorState =
  case runReactM_ReactState act rctx rst of
    Nothing -> Nothing
    Just (UndoFlag undoFlag, rst') ->
      let editorState' =
            editorState
              & esExpr .~ (rst' ^. rstNode)
              & esStack .~ (rst' ^. rstStack)
              & esMode .~ (rst' ^. rstMode)
       in Just $
            if undoFlag
              then
                editorState'
                  & esUndo %~ ((editorState ^. esExpr) :)
                  & esRedo .~ []
              else editorState'
  where
    rst =
      ReactState
        { _rstNode = editorState ^. esExpr,
          _rstStack = editorState ^. esStack,
          _rstMode = editorState ^. esMode
        }
    rctx =
      ReactCtx
        { _rctxJumptags = editorState ^. esJumptags
        }

data Action
  = ActionEscapeTransientMode
  | ActionDeleteNode Path
  | ActionPushStack Path
  | ActionPopSwapStack Path
  | ActionRotateStack
  | ActionDropStack
  | ActionDeleteCharBackward Path
  | ActionDeleteCharForward Path
  | ActionMoveStrCursorBackward Path
  | ActionMoveStrCursorForward Path
  | ActionInsertToken Path (Token Node)
  | ActionSelectParent Path
  | ActionSelectChild Path
  | ActionSelectSiblingBackward Path
  | ActionSelectSiblingForward Path
  | ActionActivateJumptags JumpAction
  | ActionJumptagLookup Char
  | ActionToggleCollapse Path
  | ActionToggleEditMode Path
  | ActionEnterInputMode
  | ActionInputSelectSymbol Path Char
  | ActionInsertTokenFromStack Path
  | ActionEnterStackInputMode

getAction ::
  WritingDirection ->
  Selection ->
  Mode ->
  InputEvent ->
  Maybe Action
getAction _ _ _ inputEvent
  -- Escape transient modes (enter normal mode)
  | KeyPress [] KeyCode.Escape <- inputEvent =
    Just ActionEscapeTransientMode
getAction wd selection ModeNormal inputEvent =
  getActionInModeNormal wd selection inputEvent
getAction _ selection ModeStack inputEvent =
  getActionInModeStack selection inputEvent
getAction _ _ (ModeJump _ _) inputEvent =
  case inputEvent of
    KeyPress [] (keyChar -> Just c) ->
      Just $ ActionJumptagLookup c
    _ -> Nothing
getAction _ Selection {selectionPath} ModeStackInput inputEvent =
  case inputEvent of
    -- Rotate the stack in input mode.
    KeyPress [] keyCode
      | keyLetter 'r' keyCode ->
        Just $ ActionRotateStack
    -- Pop a node from the stack in input mode.
    KeyPress [] keyCode
      | keyLetter 'p' keyCode ->
        Just $ ActionInsertTokenFromStack selectionPath
    _ -> Nothing
getAction _ Selection {selectionPath} (ModeInput acc _) inputEvent =
  case inputEvent of
    -- Enter stack input mode.
    KeyPress [] keyCode
      | keyLetter 'r' keyCode,
        Text.null acc ->
        Just $ ActionEnterStackInputMode
    -- Select symbol in input mode.
    KeyPress mods (keyChar -> Just c)
      | Control `notElem` mods ->
        Just $ ActionInputSelectSymbol selectionPath c
    _ -> Nothing

getActionInModeNormal ::
  WritingDirection ->
  Selection ->
  InputEvent ->
  Maybe Action
getActionInModeNormal
  wd
  Selection {selectionPath, selectionTipPos}
  inputEvent
    -- Enter edit mode from normal mode.
    | KeyPress [] KeyCode.Space <- inputEvent =
      Just (ActionToggleEditMode selectionPath)
    -- Quit from edit mode with a Space.
    -- Use Shift-Space to enter a space character.
    | Just _ <- selectionTipPos,
      KeyPress [] KeyCode.Space <- inputEvent =
      Just (ActionToggleEditMode selectionPath)
    -- Delete character backward.
    | Just _ <- selectionTipPos,
      KeyPress [] KeyCode.Backspace <- inputEvent =
      Just $ ActionDeleteCharBackward selectionPath
    -- Delete character forward.
    | Just _ <- selectionTipPos,
      KeyPress [] KeyCode.Delete <- inputEvent =
      Just $ ActionDeleteCharForward selectionPath
    -- Move string cursor backward.
    | Just _ <- selectionTipPos,
      KeyPress [] KeyCode.ArrowLeft <- inputEvent =
      Just $ ActionMoveStrCursorBackward selectionPath
    -- Move string cursor forward.
    | Just _ <- selectionTipPos,
      KeyPress [] KeyCode.ArrowRight <- inputEvent =
      Just $ ActionMoveStrCursorForward selectionPath
    -- Insert token.
    | Just _ <- selectionTipPos,
      KeyPress mods keyCode <- inputEvent,
      Control `notElem` mods,
      Just c <- keyChar keyCode =
      Just $
        case c of
          '\\' -> ActionEnterInputMode
          '_' -> ActionInsertToken selectionPath (TokenNode defaultHole)
          _ -> ActionInsertToken selectionPath (TokenChar c)
    -- Toggle node collapse.
    | KeyPress [] keyCode <- inputEvent,
      keyLetter 'c' keyCode =
      Just $ ActionToggleCollapse selectionPath
    -- Enter jumptag mode to select a node.
    | KeyPress [] keyCode <- inputEvent,
      keyLetter 'g' keyCode =
      Just $ ActionActivateJumptags JumpSelect
    -- Enter jumptag mode to copy a node.
    | KeyPress [Shift] keyCode <- inputEvent,
      keyLetter 'Y' keyCode =
      Just $ ActionActivateJumptags (JumpCopyTo selectionPath)
    -- Delete node.
    | keyCodeLetter KeyCode.Delete 'x' inputEvent =
      Just $ ActionDeleteNode selectionPath
    -- Push a node to the stack.
    | KeyPress [] keyCode <- inputEvent,
      keyLetter 'y' keyCode =
      Just $ ActionPushStack selectionPath
    -- Pop/swap a node from the stack.
    | KeyPress [] keyCode <- inputEvent,
      keyLetter 'p' keyCode =
      Just $ ActionPopSwapStack selectionPath
    -- Rotate stack.
    | KeyPress [] keyCode <- inputEvent,
      keyLetter 'r' keyCode =
      Just ActionRotateStack
    -- Select parent node.
    | keyCodeLetter KeyCode.ArrowUp 'k' inputEvent =
      Just $ ActionSelectParent selectionPath
    -- Select child node.
    | keyCodeLetter KeyCode.ArrowDown 'j' inputEvent =
      Just $ ActionSelectChild selectionPath
    -- Select sibling node left.
    | keyCodeLetter KeyCode.ArrowLeft 'h' inputEvent =
      Just $ case wd of
        WritingDirectionLTR -> ActionSelectSiblingBackward selectionPath
        WritingDirectionRTL -> ActionSelectSiblingForward selectionPath
    -- Select sibling node right.
    | keyCodeLetter KeyCode.ArrowRight 'l' inputEvent =
      Just $ case wd of
        WritingDirectionLTR -> ActionSelectSiblingForward selectionPath
        WritingDirectionRTL -> ActionSelectSiblingBackward selectionPath
getActionInModeNormal _ _ _ = Nothing

getActionInModeStack ::
  Selection ->
  InputEvent ->
  Maybe Action
getActionInModeStack Selection {selectionPath} inputEvent
  -- Enter edit mode from stack mode.
  | KeyPress [] KeyCode.Space <- inputEvent =
    Just (ActionToggleEditMode selectionPath)
  -- Drop a node from the stack.
  | KeyPress [] keyCode <- inputEvent,
    keyLetter 'x' keyCode =
    Just ActionDropStack
getActionInModeStack _ _ = Nothing

type ReactM s = WriterT UndoFlag (ReaderT ReactCtx (StateT s Maybe)) ()

runReactM_ReactState :: ReactM ReactState -> ReactCtx -> ReactState -> Maybe (UndoFlag, ReactState)
runReactM_ReactState act rctx rst =
  flip runStateT rst
    $ flip runReaderT rctx
    $ execWriterT
    $ act

applyActionM :: Action -> ReactM ReactState
applyActionM ActionEscapeTransientMode = do
  rstMode .= ModeNormal
  rstNode %= quitEditMode
applyActionM (ActionDeleteNode path) = do
  rstMode .= ModeStack
  nodes <-
    zoom (rstNode . atPath path) $ do
      node <- get
      guard (not (isHole node))
      put defaultHole
      return [node]
  forM_ nodes $ \node ->
    rstStack %= (node :)
  setUndoFlag
applyActionM (ActionPushStack path) = do
  rstMode .= ModeStack
  parent <- use rstNode
  let nodes = List.filter (not . isHole) (parent ^.. atPath path)
  forM_ nodes $ \node ->
    rstStack %= (node :)
applyActionM (ActionPopSwapStack path) = do
  rstMode .= ModeStack
  n : ns <- use rstStack
  rstStack .= ns
  popSwapNode path n
applyActionM ActionRotateStack = do
  mode <- use rstMode
  case mode of
    ModeStack -> rstStack %= rotate
    ModeStackInput -> rstStack %= rotate
    _ -> rstMode .= ModeStack
applyActionM ActionDropStack = do
  rstStack %= List.drop 1
applyActionM (ActionDeleteCharBackward path) = do
  nodes <-
    zoom (rstNode . atPath path) $ do
      Node (StrSel pos) (Syn tokens) <- get
      guard (pos > 0)
      let (before, after) = Seq.splitAt pos tokens
      (tokens', deleted) <-
        case Seq.viewr before of
          Seq.EmptyR -> A.empty
          before' Seq.:> deleted ->
            return (before' <> after, deleted)
      put $ Node (StrSel (pos - 1)) (Syn tokens')
      case deleted of
        TokenNode node | not (isHole node) -> return [node]
        _ -> return []
  forM_ nodes $ \node ->
    rstStack %= (node :)
  setUndoFlag
applyActionM (ActionDeleteCharForward path) = do
  nodes <-
    zoom (rstNode . atPath path) $ do
      Node (StrSel pos) (Syn tokens) <- get
      guard (pos < Seq.length tokens)
      let (before, after) = Seq.splitAt pos tokens
      (tokens', deleted) <-
        case Seq.viewl after of
          Seq.EmptyL -> A.empty
          deleted Seq.:< after' ->
            return (before <> after', deleted)
      put $ Node (StrSel pos) (Syn tokens')
      case deleted of
        TokenNode node | not (isHole node) -> return [node]
        _ -> return []
  forM_ nodes $ \node ->
    rstStack %= (node :)
  setUndoFlag
applyActionM (ActionMoveStrCursorBackward path) =
  zoom (rstNode . atPath path) $ do
    Node (StrSel pos) syn <- get
    guard (pos > 0)
    let pos' = pos - 1
    put $ Node (StrSel pos') syn
applyActionM (ActionMoveStrCursorForward path) =
  zoom (rstNode . atPath path) $ do
    Node (StrSel pos) syn <- get
    guard (pos < Seq.length (synTokens syn))
    let pos' = pos + 1
    put $ Node (StrSel pos') syn
applyActionM (ActionInsertToken path t) =
  zoom (rstNode . atPath path) $ do
    Node (StrSel pos) (Syn tokens) <- get
    let tokens' = Seq.insertAt pos t tokens
        pos' = pos + 1
    put $ Node (StrSel pos') (Syn tokens')
    setUndoFlag
applyActionM (ActionSelectParent path) = do
  rstMode %= quitStackMode
  path' <- maybeA (pathParent path)
  zoom (rstNode . atPath path') $ do
    Node (SynSel sel) syn <- get
    put $ Node (SynSel (toRecSelSelf sel)) syn
applyActionM (ActionSelectChild path) = do
  rstMode %= quitStackMode
  zoom (rstNode . atPath path) $ do
    Node (SynSel sel) syn <- get
    sel' <- maybeA (toRecSelChild sel)
    put $ Node (SynSel sel') syn
applyActionM (ActionToggleCollapse path) = do
  rstMode %= quitStackMode
  zoom (rstNode . atPath path) $ do
    Node (SynSel sel) syn <- get
    sel' <- maybeA (toggleNodeCollapse sel)
    put $ Node (SynSel sel') syn
applyActionM (ActionSelectSiblingBackward path) = do
  rstMode %= quitStackMode
  path' <- maybeA (pathParent path)
  zoom rstNode $ zoomPathPrefix path' $ do
    Node (SynSel (RecSel i SelChild)) syn <- get
    i' <- maybeA (indexPred i)
    put $ Node (SynSel (RecSel i' SelChild)) syn
applyActionM (ActionSelectSiblingForward path) = do
  rstMode %= quitStackMode
  path' <- maybeA (pathParent path)
  zoom rstNode $ zoomPathPrefix path' $ do
    Node (SynSel (RecSel i SelChild)) syn <- get
    let n = Foldable.length syn
    i' <- maybeA (indexSucc n i)
    put $ Node (SynSel (RecSel i' SelChild)) syn
applyActionM (ActionActivateJumptags jumpAction) = do
  Just jumptags <- views rctxJumptags nonEmpty
  rstMode .= ModeJump (withJumptagLabels jumptags) jumpAction
applyActionM (ActionJumptagLookup c) = do
  ModeJump activeJumptags jumpAction <- use rstMode
  activeJumptags' <-
    maybeA
      $ nonEmpty
      $ List.map (\(_, jt) -> jt)
      $ NonEmpty.filter (\(c', _) -> c == c')
      $ activeJumptags
  case activeJumptags' of
    Jumptag _ path :| [] -> commitJumpAction path jumpAction
    jumptags -> rstMode .= ModeJump (withJumptagLabels jumptags) jumpAction
applyActionM (ActionToggleEditMode path) = do
  rstMode .= ModeNormal
  rstNode . atPath path
    %= \(Node nodeSel syn) ->
      let nodeSel' =
            case nodeSel of
              StrSel _ -> doneEditing syn
              SynSel _ -> StrSel (Seq.length (synTokens syn))
       in Node nodeSel' syn
applyActionM ActionEnterInputMode = do
  rstMode .= ModeInput "" initialInputTrie
applyActionM ActionEnterStackInputMode = do
  rstMode .= ModeStackInput
applyActionM (ActionInputSelectSymbol path c) = do
  ModeInput acc (InputTrie t) <- use rstMode
  case Map.lookup c t of
    Nothing -> rstMode .= ModeNormal
    Just (Left t') -> rstMode .= ModeInput (acc <> Text.singleton c) t'
    Just (Right c') -> do
      rstMode .= ModeNormal
      applyActionM (ActionInsertToken path (TokenChar c'))
applyActionM (ActionInsertTokenFromStack path) = do
  rstMode .= ModeNormal
  alwaysSucceed $ do
    n : ns <- use rstStack
    rstStack .= ns
    applyActionM (ActionInsertToken path (TokenNode n))

quitEditMode :: Node -> Node
quitEditMode (Node (StrSel _) syn) = Node (doneEditing syn) syn
quitEditMode (Node nodeSel@(SynSel (RecSel i SelChild)) syn) =
  Node nodeSel (over (synIx i) quitEditMode syn)
quitEditMode node = node

synIx :: Index -> Traversal' (Syn a) a
synIx i = traversed . Lens.index (indexToInt i)

doneEditing :: Syn Node -> NodeSel
doneEditing syn =
  SynSel $
    if Foldable.null syn
      then RecSel0
      else RecSel (intToIndex 0) SelChild

withJumptagLabels :: NonEmpty b -> NonEmpty (Char, b)
withJumptagLabels = NonEmpty.zip (NonEmpty.cycle jumptagLabels)

commitJumpAction :: Path -> JumpAction -> ReactM ReactState
commitJumpAction path jumpAction = do
  alwaysSucceed $
    case jumpAction of
      JumpSelect -> do
        rstNode %= quitEditMode
        rstNode %= setPathNode path
      JumpCopyTo destinationPath -> do
        Just sourceNode <- uses rstNode (preview (atPath path))
        guard (not (isHole sourceNode))
        zoom (rstNode . atPath destinationPath) $ do
          node <- get
          guard (isHole node)
          setUndoFlag
          put sourceNode
  rstMode .= ModeNormal

popSwapNode :: Path -> Node -> ReactM ReactState
popSwapNode path n = do
  nodes <-
    zoom (rstNode . atPath path) $ do
      node <- get
      put n
      setUndoFlag
      if isHole node
        then return []
        else return [node]
  for_ nodes $ \node -> do
    rstMode .= ModeStack
    rstStack %= (node :)

rotate :: [a] -> [a]
rotate [] = []
rotate (x : xs) = xs ++ [x]

pathParent :: Path -> Maybe Path
pathParent (Path ps) =
  case List.reverse ps of
    [] -> Nothing
    _ : ps' -> Just (Path (List.reverse ps'))

atPath :: Path -> Traversal' Node Node
atPath p =
  case unconsPath p of
    Nothing -> id
    Just (ps, p') -> atPathSegment ps . atPath p'

atPathSegment :: PathSegment -> Traversal' Node Node
atPathSegment (PathSegment shape i) =
  \f node ->
    case node of
      Node sel syn | shape == synShape syn -> Node sel <$> synIx i f syn
      _ -> pure node

zoomPathPrefix :: Path -> ReactM Node -> ReactM Node
zoomPathPrefix p m =
  case unconsPath p of
    Nothing -> m
    Just (ps, p') ->
      zoom (atPathSegment ps) (zoomPathPrefix p' m) <|> m

indexPred :: Index -> Maybe Index
indexPred i =
  let i' = indexToInt i
   in if i' > 0 then Just (intToIndex (i' - 1)) else Nothing

indexSucc :: Int -> Index -> Maybe Index
indexSucc n i =
  let i' = indexToInt i + 1
   in if i' < n then Just (intToIndex i') else Nothing

--------------------------------------------------------------------------------
---- PluginInfo
--------------------------------------------------------------------------------

-- | A plugin as consumed by the editor, with additional information
-- derived from the user specification.
data PluginInfo
  = PluginInfo
      { pluginInfoSchema :: Schema,
        pluginInfoPrecInfo :: HashMap SynShape (Array PrecPredicate),
        pluginInfoShapeNames :: HashMap SynShape ShapeName
      }

mkPluginInfo :: Plugin -> PluginInfo
mkPluginInfo plugin =
  PluginInfo
    { pluginInfoSchema = pluginSchema plugin,
      pluginInfoPrecInfo = pluginPrecInfo plugin,
      pluginInfoShapeNames = pluginShapeNames plugin
    }

--------------------------------------------------------------------------------
---- Parsing
--------------------------------------------------------------------------------

fromParsedValue :: ParsedValue -> Node
fromParsedValue = go
  where
    go (ParsedValue syn) = Node (mkNodeSel syn) (fmap go syn)
    mkNodeSel syn =
      SynSel $
        if Foldable.null syn
          then RecSel0
          else RecSel (intToIndex 0) (SelSelf (Collapsed False))
