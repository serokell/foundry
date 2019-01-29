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

  -- * Draw
  module Slay.Core,
  module Slay.Combinators,
  CursorBlink(..),
  blink,
  Paths(..),
  DrawCtx(..),
  withDrawCtx,
  Draw(..),
  toCairoElementDraw,
  rect,
  rgb,
  textline,
  line,
  centerOf,
  horizontal,
  vertical,
  findPath,
  dark1, dark2, dark3, light1, white, -- colors
  textWithCursor,
  textWithoutCursor,
  punct,
  ubuntuFont,
  layoutSel,
  active,

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
  esUndo,
  esRedo,

  NodeCreateFn(..),
  ncfCheckInputEvent,
  ncfTyId,

  RecLayoutFn,

  LayoutCtx(..),
  lctxPath,
  lctxViewport,
  lctxRecLayouts,

  ReactCtx(..),
  rctxLastLayout,
  rctxInputEvent,
  rctxNodeFactory,
  rctxDefaultValues,
  rctxAllowedFieldTypes,
  rctxRecMoveMaps,

  mkDefaultValues,
  mkAllowedFieldTypes,
  mkRecMoveMaps,

  RecMoveMap,

  layoutEditorState,
  selectionPathEditorState,
  reactEditorState,

  -- * Plugin
  Plugin(..),
  pluginTyEnv,
  pluginRecLayouts,
  pluginNodeFactory,

  -- * Utils
  inj,
  nothing,
  maybeA

  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.List as List
import Data.List.NonEmpty hiding (cons)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens as Lens
import Data.Monoid
import Data.Foldable
import Inj
import Inj.Base ()

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Sequence as Seq

import Slay.Core
import Slay.Cairo.Prim.Color
import Slay.Cairo.Prim.Rect
import Slay.Cairo.Prim.Text
import Slay.Combinators
import Slay.Cairo.Element

import Source.Input
import qualified Source.Input.KeyCode as KeyCode

import Sdam.Core
  ( TyName,
    FieldName,
    TyId, mkTyId,
    FieldId, mkFieldId )

--------------------------------------------------------------------------------
---- Types
--------------------------------------------------------------------------------

newtype Env = Env { envMap :: Map TyName Ty }
  deriving newtype Show

data Ty =
  TyRec [(FieldName, TyUnion)] |
  TyStr
  deriving stock Show

data TyUnion = TyUnion (Set TyName)
  deriving stock Show

mkTyUnion :: [TyName] -> TyUnion
mkTyUnion = TyUnion . Set.fromList

instance Semigroup TyUnion where
  TyUnion tns1 <> TyUnion tns2 = TyUnion (Set.union tns1 tns2)

instance Monoid TyUnion where
  mempty = TyUnion Set.empty

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

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

newtype PathSegment = PathSegment FieldId
  deriving newtype (Eq, Show)

newtype Path = Path (Seq PathSegment)
  deriving newtype (Eq, Show)

emptyPath :: Path
emptyPath = Path Seq.empty

consPath :: PathSegment -> Path -> Path
consPath ps (Path p) = Path (ps `cons` p)

snocPath :: PathSegment -> Path -> Path
snocPath ps (Path p) = Path (p `snoc` ps)

unconsPath :: Path -> Maybe (PathSegment, Path)
unconsPath (Path p) =
  case Lens.uncons p of
    Nothing -> Nothing
    Just (ps, p') -> Just (ps, Path p')

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

instance g ~ DrawCtx => Inj (CairoElement g) Draw where
  inj = DrawCairoElement

textline :: Color -> Font -> Text -> (Paths -> CursorBlink -> Maybe Natural) -> Collage Draw
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

horizontal, vertical :: NonEmpty (Collage Draw) -> Collage Draw
horizontal = foldr1 @NonEmpty horizBaseline
vertical = foldr1 @NonEmpty vertLeft

findPath ::
  Offset ->
  NonEmpty (Positioned Draw) ->
  Maybe Path
findPath o c =
  getFirst $ foldMap (First . check) c
  where
    check (At o' d) = do
      let e = extentsOf d
      DrawEmbed _ p <- Just d
      guard $ insideBox (o', e) o
      Just p

dark1, dark2, dark3, light1, white :: Color
dark1  = RGB 41 41 41
dark2  = RGB 77 77 77
dark3  = RGB 64 64 64
light1 = RGB 179 179 179
white  = RGB 255 255 255

textWithCursor :: Text -> (Paths -> CursorBlink -> Maybe Natural) -> Collage Draw
textWithCursor = textline white ubuntuFont

textWithoutCursor :: Text -> Collage Draw
textWithoutCursor t =
  textWithCursor t (\_ _ -> Nothing)

punct :: Text -> Collage Draw
punct t = textline light1 ubuntuFont t (\_ _ -> Nothing)

ubuntuFont :: Font
ubuntuFont = Font "Ubuntu" 12 FontWeightNormal

layoutSel :: Path -> Collage Draw -> Collage Draw
layoutSel path =
    active path
  . (decorateMargin . DecorationBelow) (\e ->
      let
        mkColor color = DrawCtx $ \Paths{..} _ ->
          if pathsSelection == path then inj color else nothing
        background = rect nothing (mkColor dark3) e
        border = rect (lrtb @Natural 1 1 1 1) (mkColor (rgb 94 80 134)) e
      in
        collageCompose offsetZero background border)

active :: Path -> Collage Draw -> Collage Draw
active p = (decorateMargin . DecorationAbove) (collageSingleton . activeZone)
  where
    mkColor (Just path) | path == p = Just light1
    mkColor _ = Nothing
    outlineRect =
      rect
        (lrtb @Natural 1 1 1 1)
        (DrawCtx $ \Paths{..} _ -> mkColor pathsCursor)
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
      _esUndo :: [Holey Object],
      _esRedo :: [Holey Object]
    }

data NodeCreateFn =
  NodeCreateFn
    { _ncfCheckInputEvent :: InputEvent -> Bool,
      _ncfTyId :: TyId
    }

type RecLayoutFn =
  Map FieldId (Collage Draw) -> Collage Draw

data LayoutCtx =
  LayoutCtx
    { _lctxPath :: Path,
      _lctxViewport :: Extents,
      _lctxRecLayouts :: Map TyId RecLayoutFn
    }

data ReactCtx =
  ReactCtx
    { _rctxLastLayout :: Collage Draw,
      _rctxInputEvent :: InputEvent,
      _rctxNodeFactory :: [NodeCreateFn],
      _rctxDefaultValues :: Map TyId Value,
      _rctxAllowedFieldTypes :: Map FieldId (Set TyId),
      _rctxRecMoveMaps :: Map TyId RecMoveMap
    }

data RecMoveMap =
  RecMoveMap
    { rmmForward :: Map FieldId FieldId,
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

layoutEditorState :: LayoutCtx -> EditorState -> Collage Draw
layoutEditorState lctx es =
  withBars . centered $
    layoutHoleyObject lctx (es ^. esExpr)
  where
    hoverBar = do
      guard $ es ^. esHoverBarEnabled
      [textWithoutCursor (Text.pack . show $ es ^. esPointer)]
    bars = concat @[] [hoverBar]
    withBars =
      case nonEmpty bars of
        Nothing -> id
        Just bars' -> \c ->
          collageCompose offsetZero c (vertical bars')
    centered c =
      let
        padding = centerOf (lctx ^. lctxViewport) c
        backgroundRect = rect nothing (inj dark1)
      in
        substrate padding backgroundRect c

layoutHoleyObject :: LayoutCtx -> Holey Object -> Collage Draw
layoutHoleyObject lctx = \case
  Hole ->
    layoutSel (lctx ^. lctxPath) $
    collageWithMargin (Margin 4 4 4 4) $
    punct "_"
  Solid syn ->
    layoutObject lctx syn

layoutObject :: LayoutCtx -> Object -> Collage Draw
layoutObject lctx = \case
  Object tyId (ValueRec syn) -> layoutRec lctx tyId syn
  Object _ (ValueStr syn) -> layoutStr lctx syn

layoutStr :: LayoutCtx -> SynStr -> Collage Draw
layoutStr lctx syn =
  layoutSel (lctx ^. lctxPath) $
  collageWithMargin (Margin 4 4 4 4) $
  textWithCursor
    (syn ^. synStrContent)
    (\Paths{..} -> \case
        _ | not (syn ^. synStrEditMode) -> Nothing
        _ | pathsSelection /= lctx ^. lctxPath -> Nothing
        CursorInvisible -> Nothing
        CursorVisible -> Just . fromIntegral $ syn ^. synStrPosition)

layoutRec :: LayoutCtx -> TyId -> SynRec -> Collage Draw
layoutRec lctx tyId syn =
  layoutSel (lctx ^. lctxPath) $
  collageWithMargin (Margin 4 4 4 4) $
    let
      layoutFields :: RecLayoutFn
      layoutFields =
        case Map.lookup tyId (lctx ^. lctxRecLayouts) of
          Nothing -> \_ -> punct (Text.pack (show tyId))
          Just fn -> fn
      drawnFields :: Map FieldId (Collage Draw)
      drawnFields =
        Map.mapWithKey
          (\fieldId ->
            let
              pathSegment = PathSegment fieldId
              lctx' = lctx & lctxPath %~ snocPath pathSegment
            in
              layoutHoleyObject lctx')
          (syn ^. synRecFields)
    in
      layoutFields drawnFields

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
        pathSegment = PathSegment fieldId
        field = (syn ^. synRecFields) Map.! fieldId
        pathTail = selectionPathHoleyObject field
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
    Just (PathSegment fieldId, path') ->
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

reactEditorState :: ReactCtx -> EditorState -> IO (Maybe EditorState)
reactEditorState = runReactM (asum handlers)
  where
    handlers :: [ReactM EditorState ()]
    handlers =
      [ handlePointerMotion,
        handleButtonPress,
        handleCtrl_h,
        handleRedirectExpr,
        handleCtrl_z,
        handleCtrl_r ]
    handlePointerMotion = do
      PointerMotion x y <- view rctxInputEvent
      esPointer .= Offset (fromIntegral x) (fromIntegral y)
    handleButtonPress = do
      ButtonPress <- view rctxInputEvent
      pointer <- use esPointer
      Just p <-
        findPath pointer . collageElements offsetZero <$>
          view rctxLastLayout
      liftIO (print p)
      modify (updatePathEditorState p)
    handleCtrl_h = do
      KeyPress [Control] keyCode <- view rctxInputEvent
      guard $ keyLetter 'h' keyCode
      esHoverBarEnabled %= not
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

mkDefaultValues :: Env -> Map TyId Value
mkDefaultValues env =
  Map.fromList
    [ (mkTyId tyName, mkDefVal tyName ty) |
      (tyName, ty) <- Map.toList (envMap env) ]
  where
    mkDefVal :: TyName -> Ty -> Value
    mkDefVal tyName = \case
      TyStr -> ValueStr (SynStr "" 0 True)
      TyRec fieldTys ->
        let
          fields = Map.fromList
            [ (mkFieldId tyName fieldName, Hole) |
              (fieldName, _) <- fieldTys ]
          sel =
            case fieldTys of
              [] -> RecSelSelf SelfSelEmpty
              (fieldName, _):_ ->
                RecSelChild (mkFieldId tyName fieldName)
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
      let moveMap = rmmBackward (recMoveMaps Map.! recTyId)
      fieldId' <- maybeA (Map.lookup fieldId moveMap)
      synRecSel .= RecSelChild fieldId'
      return (UndoFlag False)
    handleArrowRight :: ReactM SynRec UndoFlag
    handleArrowRight = do
      guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
      RecSelChild fieldId <- use synRecSel
      recMoveMaps <- view rctxRecMoveMaps
      let moveMap = rmmForward (recMoveMaps Map.! recTyId)
      fieldId' <- maybeA (Map.lookup fieldId moveMap)
      synRecSel .= RecSelChild fieldId'
      return (UndoFlag False)

mkAllowedFieldTypes :: Env -> Map FieldId (Set TyId)
mkAllowedFieldTypes env =
  Map.fromList
    [ (mkFieldId tyName fieldName, Set.map mkTyId tys) |
      (tyName, TyRec fields) <- Map.toList (envMap env),
      (fieldName, TyUnion tys) <- fields ]

mkRecMoveMaps :: Env -> Map TyId RecMoveMap
mkRecMoveMaps env =
  Map.fromList
    [ (mkTyId tyName, mkRecMoveMap tyName fields) |
      (tyName, TyRec fields) <- Map.toList (envMap env) ]

mkRecMoveMap :: TyName -> [(FieldName, u)] -> RecMoveMap
mkRecMoveMap tyName fields =
  RecMoveMap
    { rmmForward = seqToMoveMap fieldIds,
      rmmBackward = seqToMoveMap (List.reverse fieldIds) }
  where
    seqToMoveMap xs =
      case xs of
        [] -> Map.empty
        _:xs' -> Map.fromList (List.zip xs xs')
    fieldIds =
      [ mkFieldId tyName fieldName |
        (fieldName, _) <- fields ]

--------------------------------------------------------------------------------
---- Plugin
--------------------------------------------------------------------------------

data Plugin =
  Plugin
    { _pluginTyEnv :: Env,
      _pluginRecLayouts :: Map TyId RecLayoutFn,
      _pluginNodeFactory :: [NodeCreateFn]
    }

makeLenses ''Plugin
