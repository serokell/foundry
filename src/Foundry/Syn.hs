{-# OPTIONS -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundry.Syn where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Void
import Data.Typeable

import Control.Monad.State
import Control.Monad.Except
import Control.Lens

import Source.Collage.Builder (horizontal, vertical)
import Source.Syntax
import Source.Draw
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Foundry.Syn.Text
import Foundry.Syn.Common
import Foundry.Syn.TH

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

data EXPR_K = LAM | PI | APP | CONST | EMBED | VAR
data ARG
data EXPR
data TOP (n :: *) (m :: *)
data HOLE sub

data instance SEL (HOLE sub) = SelHole (SEL sub)
  deriving (Eq, Ord, Show)

data instance SEL LAM = SelLamArg | SelLamExpr1 | SelLamExpr2
  deriving (Eq, Ord, Show)

data instance SEL PI = SelPiArg | SelPiExpr1 | SelPiExpr2
  deriving (Eq, Ord, Show)

data instance SEL APP = SelAppExpr1 | SelAppExpr2
  deriving (Eq, Ord, Show)

data instance SEL CONST = SelConst { unSelConst :: Void }
  deriving (Eq, Ord, Show)

data instance SEL EMBED = SelEmbed { unSelEmbed :: Void }
  deriving (Eq, Ord, Show)

data instance SEL ARG = SelArg { unSelArg :: Void }
  deriving (Eq, Ord, Show)

data instance SEL VAR = SelVar { unSelVar :: Void }
  deriving (Eq, Ord, Show)

declarePrisms
  [d|
    data instance SYN (HOLE sub) = SynSolid (SYN sub) | SynHollow
      deriving (Eq, Ord, Show)
  |]

data instance SYN LAM = SynLam
  { _synLamArg   :: SYN ARG
  , _synLamExpr1 :: SYN (HOLE EXPR)
  , _synLamExpr2 :: SYN (HOLE EXPR)
  , _synLamSel   :: Maybe (SEL LAM)
  } deriving (Eq, Ord, Show)

data instance SYN PI = SynPi
  { _synPiArg   :: SYN ARG
  , _synPiExpr1 :: SYN (HOLE EXPR)
  , _synPiExpr2 :: SYN (HOLE EXPR)
  , _synPiSel   :: Maybe (SEL PI)
  } deriving (Eq, Ord, Show)

data instance SYN APP = SynApp
  { _synAppExpr1 :: SYN (HOLE EXPR)
  , _synAppExpr2 :: SYN (HOLE EXPR)
  , _synAppSel   :: Maybe (SEL APP)
  } deriving (Eq, Ord, Show)

data instance SYN CONST = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

data instance SYN EMBED
  = SynEmbedFilePath (SYN TEXT)
  | SynEmbedURL (SYN TEXT)
  deriving (Eq, Ord, Show)

newtype instance SYN ARG = SynArg (SYN TEXT)
  deriving (Eq, Ord, Show)

data instance SYN VAR = SynVar
  { _synVarName  :: SYN TEXT
  , _synVarIndex :: Int
  } deriving (Eq, Ord, Show)

data instance SEL EXPR
  = SelExprLam (SEL LAM)
  | SelExprPi  (SEL PI)
  | SelExprApp (SEL APP)
  deriving (Eq, Ord, Show)

data instance SYN EXPR
  = SynExprLam   (SYN LAM)
  | SynExprPi    (SYN PI)
  | SynExprApp   (SYN APP)
  | SynExprConst (SYN CONST)
  | SynExprVar   (SYN VAR)
  | SynExprEmbed (SYN EMBED)
  deriving (Eq, Ord, Show)

declareLenses
  [d|
      data instance SYN (TOP n m) = SynTop
        { synExpr            :: SYN (HOLE EXPR)
        , synPointer         :: Offset n m
        , synHoverBarEnabled :: Bool
        , synUndo            :: [SYN (HOLE EXPR)]
        , synRedo            :: [SYN (HOLE EXPR)]
        } deriving (Eq, Ord, Show)
  |]

makeLensesDataInst ''SYN 'VAR
makePrismsDataInst ''SYN ''ARG
makeLensesDataInst ''SYN 'LAM
makeLensesDataInst ''SYN 'PI
makeLensesDataInst ''SYN 'APP
makePrismsDataInst ''SYN ''EXPR

synImportExpr :: M.Expr M.X -> SYN EXPR
synImportExpr =
  let
    synImportText t =
      let t' = Text.Lazy.toStrict t
      in SynText t' (Text.length t') False
    synImportConst = \case
      M.Star -> SynConstStar
      M.Box  -> SynConstBox
    synImportVar (M.V t n) = SynVar (synImportText t) n
    synImportArg t = SynArg (synImportText t)
    synImportLam x _A  b = SynLam
      (synImportArg x)
      (SynSolid (synImportExpr _A))
      (SynSolid (synImportExpr b))
      Nothing
    synImportPi  x _A _B = SynPi
      (synImportArg x)
      (SynSolid (synImportExpr _A))
      (SynSolid (synImportExpr _B))
      Nothing
    synImportApp f a = SynApp
      (SynSolid (synImportExpr f))
      (SynSolid (synImportExpr a))
      Nothing
  in \case
    M.Const c -> SynExprConst $ synImportConst c
    M.Var   v -> SynExprVar   $ synImportVar   v
    M.Lam x _A  b -> SynExprLam $ synImportLam x _A  b
    M.Pi  x _A _B -> SynExprPi  $ synImportPi  x _A _B
    M.App f a -> SynExprApp $ synImportApp f a
    M.Embed e -> M.absurd e

class SynSelection a where
  synSelection :: SYN a -> Maybe (SEL a)
  synSelection = const Nothing

synSelectionSelf :: SynSelection a => SYN a -> Bool
synSelectionSelf = isNothing . synSelection

instance SynSelection LAM where
  synSelection = view synLamSel

instance SynSelection PI where
  synSelection = view synPiSel

instance SynSelection APP where
  synSelection = view synAppSel

instance SynSelection ARG
instance SynSelection CONST
instance SynSelection VAR
instance SynSelection EMBED

instance SynSelection EXPR where
  synSelection = \case
    SynExprLam   a -> SelExprLam <$> synSelection a
    SynExprPi    a -> SelExprPi  <$> synSelection a
    SynExprApp   a -> SelExprApp <$> synSelection a
    SynExprConst a -> absurd . unSelConst <$> synSelection a
    SynExprVar   a -> absurd . unSelVar   <$> synSelection a
    SynExprEmbed a -> absurd . unSelEmbed <$> synSelection a

instance SynSelection sub => SynSelection (HOLE sub) where
  synSelection = \case
    SynHollow -> Nothing
    SynSolid syn -> SelHole <$> synSelection syn

---       Arg       ---
---    instances    ---

instance UndoEq (SYN ARG) where
  undoEq (SynArg s1) (SynArg s2) = undoEq s1 s2

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN ARG) where

  layout lctx (SynArg t) = layout lctx t

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN ARG) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers =
        [ handleRedirect
        , handle_x
        ]
      handleRedirect = zoom _SynArg $ do
        react (asyncReact . over _SynArg) oldLayout inputEvent
      handle_x = do
        guard $ keyCodeLetter KeyCode.Delete 'x' inputEvent
        _SynArg .= mempty


---       Lam       ---
---    instances    ---

instance UndoEq (SYN LAM) where
  undoEq s1 s2
     = on undoEq (view synLamArg)   s1 s2
    && on undoEq (view synLamExpr1) s1 s2
    && on undoEq (view synLamExpr2) s1 s2

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN LAM) where

  layout lctx syn =
    let
      maxWidth = (max `on` fst.getExtents) header body
      header =
        [ extend (4, 0) (punct "λ")
        , [ selLayout lctx (SelLamArg, view synLamArg) (join pad (4, 0)) syn
          , join pad (4, 0) (punct ":")
          , selLayout lctx (SelLamExpr1, view synLamExpr1) (join pad (4, 0)) syn
          ] & horizontal
        ] & horizontal
      body = selLayout lctx (SelLamExpr2, view synLamExpr2) id syn
    in
      [ header
      , join pad (0, 4) (line light1 maxWidth)
      , body
      ] & vertical

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN LAM) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SYN LAM) (ExceptT () IO) ()]
      handlers =
        [ handleSelRedirect
        , handleArrowLeft
        , handleArrowRight
        , handleArrowUp
        , handleArrowDown
        ]
      handleArrowUp = do
        guard $ keyCodeLetter KeyCode.ArrowUp 'k' inputEvent
        guard =<< gets (isJust . synSelection)
        synLamSel .= Nothing
      handleArrowDown = do
        guard $ keyCodeLetter KeyCode.ArrowDown 'j' inputEvent
        selection <- gets synSelection
        guard $ isNothing selection
        synLamSel .= Just SelLamExpr2
      handleArrowLeft = do
        guard $ keyCodeLetter KeyCode.ArrowLeft 'h' inputEvent
        use synLamSel >>= \case
          Just SelLamExpr2 -> synLamSel . _Just .= SelLamExpr1
          Just SelLamExpr1 -> synLamSel . _Just .= SelLamArg
          _ -> mzero
      handleArrowRight = do
        guard $ keyCodeLetter KeyCode.ArrowRight 'l' inputEvent
        use synLamSel >>= \case
          Just SelLamArg   -> synLamSel . _Just .= SelLamExpr1
          Just SelLamExpr1 -> synLamSel . _Just .= SelLamExpr2
          _ -> mzero
      handleSelRedirect = do
        let
          delegate
            :: forall syn
             . SyntaxReact n m (CollageDraw' n m) syn
            => Lens' (SYN LAM) syn -> StateT (SYN LAM) (ExceptT () IO) ()
          delegate p = zoom p $ do
            react (asyncReact . over p) oldLayout inputEvent
        selection <- gets synSelection
        case selection of
          Nothing          -> throwError ()
          Just SelLamArg   -> delegate synLamArg
          Just SelLamExpr1 -> delegate synLamExpr1
          Just SelLamExpr2 -> delegate synLamExpr2

instance SyntaxBlank (SYN LAM) where
  blank = return SynLam
    { _synLamArg   = SynArg mempty
    , _synLamExpr1 = SynHollow
    , _synLamExpr2 = SynHollow
    , _synLamSel   = Just SelLamArg
    }


---        Pi       ---
---    instances    ---

instance UndoEq (SYN PI) where
  undoEq s1 s2
     = on undoEq (view synPiArg)   s1 s2
    && on undoEq (view synPiExpr1) s1 s2
    && on undoEq (view synPiExpr2) s1 s2

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN PI) where

  layout lctx syn =
    let
      maxWidth = (max `on` fst.getExtents) header body
      header =
        [ extend (4, 0) (punct "Π")
        , [ selLayout lctx (SelPiArg, view synPiArg) (join pad (4, 0)) syn
          , join pad (4, 0) (punct ":")
          , selLayout lctx (SelPiExpr1, view synPiExpr1) (join pad (4, 0)) syn
          ] & horizontal
        ] & horizontal
      body = selLayout lctx (SelPiExpr2, view synPiExpr2) id syn
    in
      [ header
      , join pad (0, 4) (line light1 maxWidth)
      , body
      ] & vertical

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN PI) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SYN PI) (ExceptT () IO) ()]
      handlers =
        [ handleSelRedirect
        , handleArrowLeft
        , handleArrowRight
        , handleArrowUp
        , handleArrowDown
        ]
      handleArrowUp = do
        guard $ keyCodeLetter KeyCode.ArrowUp 'k' inputEvent
        guard =<< gets (isJust . synSelection)
        synPiSel .= Nothing
      handleArrowDown = do
        guard $ keyCodeLetter KeyCode.ArrowDown 'j' inputEvent
        selection <- gets synSelection
        guard $ isNothing selection
        synPiSel .= Just SelPiExpr2
      handleArrowLeft = do
        guard $ keyCodeLetter KeyCode.ArrowLeft 'h' inputEvent
        use synPiSel >>= \case
          Just SelPiExpr2 -> synPiSel . _Just .= SelPiExpr1
          Just SelPiExpr1 -> synPiSel . _Just .= SelPiArg
          _ -> mzero
      handleArrowRight = do
        guard $ keyCodeLetter KeyCode.ArrowRight 'l' inputEvent
        use synPiSel >>= \case
          Just SelPiArg   -> synPiSel . _Just .= SelPiExpr1
          Just SelPiExpr1 -> synPiSel . _Just .= SelPiExpr2
          _ -> mzero
      handleSelRedirect = do
        let
          delegate
            :: forall syn
             . SyntaxReact n m (CollageDraw' n m) syn
            => Lens' (SYN PI) syn -> StateT (SYN PI) (ExceptT () IO) ()
          delegate p = zoom p $ do
            react (asyncReact . over p) oldLayout inputEvent
        selection <- gets synSelection
        case selection of
          Nothing         -> throwError ()
          Just SelPiArg   -> delegate synPiArg
          Just SelPiExpr1 -> delegate synPiExpr1
          Just SelPiExpr2 -> delegate synPiExpr2

instance SyntaxBlank (SYN PI) where
  blank = return SynPi
    { _synPiArg   = SynArg mempty
    , _synPiExpr1 = SynHollow
    , _synPiExpr2 = SynHollow
    , _synPiSel   = Just SelPiArg
    }


---       App       ---
---    instances    ---

instance UndoEq (SYN APP) where
  undoEq s1 s2
     = on undoEq (view synAppExpr1) s1 s2
    && on undoEq (view synAppExpr2) s1 s2

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN APP) where

  layout lctx syn =
        [ selLayout lctx (SelAppExpr1, view synAppExpr1) (join pad (5, 5)) syn
        , join pad (5, 5)
          $ selLayout lctx
              (SelAppExpr2, view synAppExpr2)
              (outline dark2 . join pad (5, 5))
              syn
        ] & horizontalCenter

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN APP) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SYN APP) (ExceptT () IO) ()]
      handlers =
        [ handleSelRedirect
        , handleArrowLeft
        , handleArrowRight
        , handleArrowUp
        , handleArrowDown
        ]
      handleArrowUp = do
        guard $ keyCodeLetter KeyCode.ArrowUp 'k' inputEvent
        guard . isJust =<< gets synSelection
        synAppSel .= Nothing
      handleArrowDown = do
        guard $ keyCodeLetter KeyCode.ArrowDown 'j' inputEvent
        guard . isNothing =<< gets synSelection
        synAppSel .= Just SelAppExpr1
      handleArrowLeft = do
        guard $ keyCodeLetter KeyCode.ArrowLeft 'h' inputEvent
        use synAppSel >>= \case
          Just SelAppExpr2 -> synAppSel . _Just .= SelAppExpr1
          _ -> mzero
      handleArrowRight = do
        guard $ keyCodeLetter KeyCode.ArrowRight 'l' inputEvent
        use synAppSel >>= \case
          Just SelAppExpr1 -> synAppSel . _Just .= SelAppExpr2
          _ -> mzero
      handleSelRedirect = do
        let
          delegate
            :: forall syn
             . SyntaxReact n m (CollageDraw' n m) syn
            => Lens' (SYN APP) syn -> StateT (SYN APP) (ExceptT () IO) ()
          delegate p = zoom p $ do
            react (asyncReact . over p) oldLayout inputEvent
        selection <- gets synSelection
        case selection of
          Nothing          -> throwError ()
          Just SelAppExpr1 -> delegate synAppExpr1
          Just SelAppExpr2 -> delegate synAppExpr2

instance SyntaxBlank (SYN APP) where
  blank = return SynApp
    { _synAppExpr1 = SynHollow
    , _synAppExpr2 = SynHollow
    , _synAppSel   = Just SelAppExpr1
    }


---      Const      ---
---    instances    ---

instance UndoEq (SYN CONST) where
  undoEq = (==)

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN CONST) where

  layout _ = \case
    SynConstStar -> punct "★"
    SynConstBox  -> punct "□"

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN CONST) where

---       Var       ---
---    instances    ---

instance UndoEq (SYN VAR) where
  undoEq s1 s2
     = on undoEq (view synVarName)  s1 s2
    && on (==)   (view synVarIndex) s1 s2

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN VAR) where

  layout lctx v =
    [ layout lctx (v ^. synVarName)
    , if (v ^. synVarIndex) > 0
      then layoutIndex (v ^. synVarIndex)
      else
        mempty
    ] & horizontal
    where
      layoutIndex = text . Text.map toSub . Text.pack . show
      toSub :: Char -> Char
      toSub = \case
        '0' -> '₀'
        '1' -> '₁'
        '2' -> '₂'
        '3' -> '₃'
        '4' -> '₄'
        '5' -> '₅'
        '6' -> '₆'
        '7' -> '₇'
        '8' -> '₈'
        '9' -> '₉'
        c   -> c

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN VAR) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SYN VAR) (ExceptT () IO) ()]
      handlers =
        [ handleShiftUp
        , handleShiftDown
        , handleRedirect
        ]
      handleShiftUp =
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'U' keyCode
            -> synVarIndex += 1
          _ -> throwError ()
      handleShiftDown =
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'D' keyCode
            -> synVarIndex %= max 0 . subtract 1
          _ -> throwError ()
      handleRedirect = zoom synVarName $ do
        react (asyncReact . over synVarName) oldLayout inputEvent

instance SyntaxBlank (SYN VAR) where
  blank = return $ SynVar mempty 0

---      Embed      ---
---    instances    ---

instance UndoEq (SYN EMBED) where
  undoEq (SynEmbedFilePath t1) (SynEmbedFilePath t2) = undoEq t1 t2
  undoEq (SynEmbedURL      t1) (SynEmbedURL      t2) = undoEq t1 t2
  undoEq  _                     _                    = False

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN EMBED) where

  layout _ _ = text "Embed"

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN EMBED) where

---       Expr      ---
---    instances    ---

instance UndoEq (SYN EXPR) where
  undoEq (SynExprLam   a1) (SynExprLam   a2) = undoEq a1 a2
  undoEq (SynExprPi    a1) (SynExprPi    a2) = undoEq a1 a2
  undoEq (SynExprApp   a1) (SynExprApp   a2) = undoEq a1 a2
  undoEq (SynExprConst a1) (SynExprConst a2) = undoEq a1 a2
  undoEq (SynExprVar   a1) (SynExprVar   a2) = undoEq a1 a2
  undoEq (SynExprEmbed a1) (SynExprEmbed a2) = undoEq a1 a2
  undoEq  _                 _                = False

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx (SYN EXPR) where

  layout lctx = \case
    SynExprLam   a -> layout lctx a
    SynExprPi    a -> layout lctx a
    SynExprApp   a -> layout lctx a
    SynExprConst a -> layout lctx a
    SynExprVar   a -> layout lctx a
    SynExprEmbed a -> layout lctx a

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN EXPR) where

  react asyncReact oldLayout inputEvent =
    let
      delegate
        :: SyntaxReact n m (CollageDraw' n m) syn
        => Prism' (SYN EXPR) syn -> StateT (SYN EXPR) (ExceptT () IO) ()
      delegate p =
        let asyncReact' = asyncReact . over p
        in do
          guard =<< gets (notNullOf p)
          zoom p $ react asyncReact' oldLayout inputEvent
    in asum
     [ delegate _SynExprLam
     , delegate _SynExprPi
     , delegate _SynExprApp
     , delegate _SynExprConst
     , delegate _SynExprVar
     , delegate _SynExprEmbed
     ]

  subreact _asyncReact _oldLayout inputEvent = asum handlers
    where
      handlers :: [ExceptT () IO (SYN EXPR)]
      handlers =
        [ handleShift_L
        , handleShift_P
        , handleShift_A
        , handleShift_S
        , handleShift_B
        , handle_i
        -- , handleShift_E
        ]
      handleShift_L = do
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'L' keyCode
            -> SynExprLam <$> liftIO blank
          _ -> throwError ()
      handleShift_P = do
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'P' keyCode
            -> SynExprPi <$> liftIO blank
          _ -> throwError ()
      handleShift_A = do
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'A' keyCode
            -> SynExprApp <$> liftIO blank
          _ -> throwError ()
      handleShift_S = do
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'S' keyCode
            -> return $ SynExprConst SynConstStar
          _ -> throwError ()
      handleShift_B = do
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'B' keyCode
            -> return $ SynExprConst SynConstBox
          _ -> throwError ()
      handle_i = do
        case inputEvent of
          KeyPress [] keyCode | keyLetter 'i' keyCode
            -> SynExprVar <$> liftIO blank
          _ -> throwError ()


---       Hole      ---
---    instances    ---

instance UndoEq (SYN sub) => UndoEq (SYN (HOLE sub)) where
  undoEq (SynSolid s1) (SynSolid s2) = undoEq s1 s2
  undoEq  SynHollow     SynHollow    = True
  undoEq  _             _            = False

instance
  ( n ~ Int, m ~ Int
  , SyntaxLayout n m (CollageDraw' n m) lctx (SYN sub)
  ) => SyntaxLayout n m (CollageDraw' n m) lctx (SYN (HOLE sub)) where

  layout lctx = \case
    SynHollow    -> punct "⦿"
    SynSolid syn -> layout lctx syn

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  , SyntaxReact n m (CollageDraw' n m) (SYN sub)
  ) => SyntaxReact n m (CollageDraw' n m) (SYN (HOLE sub)) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SYN (HOLE sub)) (ExceptT () IO) ()]
      handlers =
        [ handleRedirectHollow
        , handleRedirectSolid
        , handleDelete
        ]
      handleRedirectHollow = do
        synh <- get
        let asyncReact' = asyncReact . over _SynSolid
        case synh of
          SynHollow -> do
            syn <- lift $ subreact asyncReact' oldLayout inputEvent
            put (SynSolid syn)
          _ -> throwError ()
      handleRedirectSolid = do
        guard =<< gets (notNullOf _SynSolid)
        zoom _SynSolid $ do
          let asyncReact' = asyncReact . over _SynSolid
          react asyncReact' oldLayout inputEvent
      handleDelete = do
        guard $ keyCodeLetter KeyCode.Delete 'x' inputEvent
        put SynHollow

instance SyntaxBlank (SYN (HOLE sub)) where
  blank = return SynHollow


---       Syn       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) (Extents n m) (SYN (TOP n m)) where

  layout viewport syn =
    let
      hoverBar = do
        guard $ syn ^. synHoverBarEnabled
        [text . Text.pack . show $ syn ^. synPointer]
      bars = concat [hoverBar]
      lctx = LayoutCtx True Seq.empty
    in flip mappend (vertical bars)
     . hover (outline light1) (syn ^. synPointer)
     . background dark1
     . center viewport
     . sel (lctx & lctxSelected &&~ synSelectionSelf (syn ^. synExpr))
     . join pad (5, 5)
     $ layout lctx (syn ^. synExpr)

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) (SYN (TOP n m)) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SYN (TOP n m)) (ExceptT () IO) ()]
      handlers =
        [ handlePointerMotion
        , handleButtonPress
        , handleCtrl_h
        , handleRedirectExpr
        , handle_u
        , handleCtrl_r
        ]
      handlePointerMotion = do
        case inputEvent of
          PointerMotion x y
            -> synPointer .= (x, y)
          _ -> throwError ()
      handleButtonPress = do
        case inputEvent of
          ButtonPress -> do
            pointer <- use synPointer
            let mp = activate (\_ _ p -> p) pointer oldLayout
            case mp of
              Nothing -> throwError ()
              Just p -> zoom synExpr $ modify (updateExprPath p)
          _ -> throwError ()
      handleCtrl_h = do
        case inputEvent of
          KeyPress [Control] keyCode | keyLetter 'h' keyCode
            -> synHoverBarEnabled %= not
          _ -> throwError ()
      handle_u = do
        case inputEvent of
          KeyPress [] keyCode | keyLetter 'u' keyCode -> do
            syn <- get
            case syn ^. synUndo of
              [] -> mzero
              (u:us) -> do
                synRedo %= (view synExpr syn :)
                synUndo .= us
                synExpr .= u
          _ -> throwError ()
      handleCtrl_r = do
        case inputEvent of
          KeyPress [Control] keyCode | keyLetter 'r' keyCode -> do
            syn <- get
            case syn ^. synRedo of
              [] -> mzero
              (r:rs) -> do
                synUndo %= (view synExpr syn :)
                synRedo .= rs
                synExpr .= r
          _ -> throwError ()
      handleRedirectExpr = do
        expr <- use synExpr
        zoom synExpr $ do
          react
            (asyncReact . over synExpr)
            oldLayout
            inputEvent
        expr' <- use synExpr
        unless (undoEq expr expr') $ do
          synUndo %= (expr:)
          synRedo .= []

updateExprPath :: Path -> SYN (HOLE EXPR) -> SYN (HOLE EXPR)
updateExprPath path = \case
  SynHollow -> SynHollow
  SynSolid e -> SynSolid $ case e of
    SynExprLam   a -> SynExprLam (updateLamPath path a)
    SynExprPi    a -> SynExprPi  (updatePiPath  path a)
    SynExprApp   a -> SynExprApp (updateAppPath path a)
    SynExprConst _ -> e
    SynExprVar   _ -> e
    SynExprEmbed _ -> e

updateLamPath :: Path -> SYN LAM -> SYN LAM
updateLamPath path e =
  case uncons path of
    Nothing -> e & synLamSel .~ Nothing
    Just (SomeSel sel', someSels) ->
      let msel = cast sel'
      in e
       & synLamSel .~ msel
       & case msel of
          Just SelLamExpr1 -> synLamExpr1 %~ updateExprPath someSels
          Just SelLamExpr2 -> synLamExpr2 %~ updateExprPath someSels
          _ -> id

updatePiPath :: Path -> SYN PI -> SYN PI
updatePiPath path e =
  case uncons path of
    Nothing -> e & synPiSel .~ Nothing
    Just (SomeSel sel', someSels) ->
      let msel = cast sel'
      in e
       & synPiSel .~ msel
       & case msel of
          Just SelPiExpr1 -> synPiExpr1 %~ updateExprPath someSels
          Just SelPiExpr2 -> synPiExpr2 %~ updateExprPath someSels
          _ -> id

updateAppPath :: Path -> SYN APP -> SYN APP
updateAppPath path e =
  case uncons path of
    Nothing -> e & synAppSel .~ Nothing
    Just (SomeSel sel', someSels) ->
      let msel = cast sel'
      in e
       & synAppSel .~ msel
       & case msel of
          Nothing -> id
          Just SelAppExpr1 -> synAppExpr1 %~ updateExprPath someSels
          Just SelAppExpr2 -> synAppExpr2 %~ updateExprPath someSels

instance (n ~ Int, m ~ Int) => SyntaxBlank (SYN (TOP n m)) where
  blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    expr <- synImportExpr <$> case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> M.I.load e
    return $ SynTop (SynSolid expr) (0, 0) False [] []


---  helpers  ---

selLayout lctx (sel', synSub) hook syn =
  let
    lctx'
      = lctx
      & lctxSelected &&~ (synSelection syn == Just sel')
      & lctxPath %~ (`snoc` SomeSel sel')
  in sel (lctx' & lctxSelected &&~ synSelectionSelf (synSub syn))
   $ hook
   $ layout lctx' (synSub syn)
