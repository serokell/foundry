{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Source.Language.Morte.Syn where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Void
import Data.Typeable

import Control.Monad.State
import Control.Monad.Except
import Control.Lens

import Source.Syntax
import Source.Draw
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Source.Language.Morte.Syn.Text
import Source.Language.Morte.Syn.Common

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

data instance Sel (SynHole syn) = SelHole (Sel syn)

data SynHole syn = SynSolid syn | SynHollow
  deriving (Eq, Ord, Show)

data instance Sel SynLam = SelLamArg | SelLamExpr1 | SelLamExpr2
  deriving (Eq, Ord, Show)

data SynLam = SynLam
  { _synLamArg   :: SynArg
  , _synLamExpr1 :: SynHole SynExpr
  , _synLamExpr2 :: SynHole SynExpr
  , _synLamSel   :: Maybe (Sel SynLam)
  } deriving (Eq, Ord, Show)

data instance Sel SynPi = SelPiArg | SelPiExpr1 | SelPiExpr2
  deriving (Eq, Ord, Show)

data SynPi = SynPi
  { _synPiArg   :: SynArg
  , _synPiExpr1 :: SynHole SynExpr
  , _synPiExpr2 :: SynHole SynExpr
  , _synPiSel   :: Maybe (Sel SynPi)
  } deriving (Eq, Ord, Show)

data instance Sel SynApp = SelAppExpr1 | SelAppExpr2
  deriving (Eq, Ord, Show)

data SynApp = SynApp
  { _synAppExpr1 :: SynHole SynExpr
  , _synAppExpr2 :: SynHole SynExpr
  , _synAppSel   :: Maybe (Sel SynApp)
  } deriving (Eq, Ord, Show)

data instance Sel SynConst = SelConst { unSelConst :: Void }

data SynConst = SynConstStar | SynConstBox
  deriving (Eq, Ord, Show)

data instance Sel SynEmbed = SelEmbed { unSelEmbed :: Void }

data SynEmbed
  = SynEmbedFilePath SynText
  | SynEmbedURL SynText
  deriving (Eq, Ord, Show)

data instance Sel SynArg = SelArg { unSelArg :: Void }

newtype SynArg = SynArg SynText
  deriving (Eq, Ord, Show)

data instance Sel SynVar = SelVar { unSelVar :: Void }

data SynVar = SynVar
  { _synVarName  :: SynText
  , _synVarIndex :: SynInt
  } deriving (Eq, Ord, Show)

newtype SynInt = SynInt Int
  deriving (Eq, Ord, Show)

data instance Sel SynExpr
  = SelExprLam (Sel SynLam)
  | SelExprPi  (Sel SynPi)
  | SelExprApp (Sel SynApp)
  deriving (Eq, Ord, Show)

data SynExpr
  = SynExprLam   SynLam
  | SynExprPi    SynPi
  | SynExprApp   SynApp
  | SynExprConst SynConst
  | SynExprVar   SynVar
  | SynExprEmbed SynEmbed
  deriving (Eq, Ord, Show)

data Syn n m = Syn
  { _synExpr            :: SynHole SynExpr
  , _synPointer         :: Offset n m
  , _synHoverBarEnabled :: Bool
  , _synUndo            :: [SynHole SynExpr]
  , _synRedo            :: [SynHole SynExpr]
  } deriving (Eq, Ord, Show)

makePrisms ''SynInt
makeLenses ''SynVar
makePrisms ''SynArg
makeLenses ''SynLam
makeLenses ''SynPi
makeLenses ''SynApp
makePrisms ''SynExpr
makePrisms ''SynHole
makeLenses ''Syn

synImportExpr :: M.Expr M.X -> SynExpr
synImportExpr =
  let
    synImportText t =
      let t' = Text.Lazy.toStrict t
      in SynText t' (Text.length t') False
    synImportInt n = SynInt n
    synImportConst = \case
      M.Star -> SynConstStar
      M.Box  -> SynConstBox
    synImportVar (M.V t n) = SynVar (synImportText t) (synImportInt n)
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
  synSelection :: a -> Maybe (Sel a)
  synSelection = const Nothing

synSelectionSelf :: SynSelection a => a -> Bool
synSelectionSelf = isNothing . synSelection

instance SynSelection SynLam where
  synSelection = view synLamSel

instance SynSelection SynPi where
  synSelection = view synPiSel

instance SynSelection SynApp where
  synSelection = view synAppSel

instance SynSelection SynArg
instance SynSelection SynConst
instance SynSelection SynVar
instance SynSelection SynEmbed

instance SynSelection SynExpr where
  synSelection = \case
    SynExprLam   a -> SelExprLam <$> synSelection a
    SynExprPi    a -> SelExprPi  <$> synSelection a
    SynExprApp   a -> SelExprApp <$> synSelection a
    SynExprConst a -> absurd . unSelConst <$> synSelection a
    SynExprVar   a -> absurd . unSelVar   <$> synSelection a
    SynExprEmbed a -> absurd . unSelEmbed <$> synSelection a

instance SynSelection syn => SynSelection (SynHole syn) where
  synSelection = \case
    SynHollow -> Nothing
    SynSolid syn -> SelHole <$> synSelection syn

---       Arg       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynArg where

  layout lctx (SynArg t) = layout lctx t

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) SynArg where

  react asyncReact oldLayout inputEvent = zoom _SynArg $ do
    react (asyncReact . over _SynArg) oldLayout inputEvent


---       Lam       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynLam where

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
  ) => SyntaxReact n m (CollageDraw' n m) SynLam where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT SynLam (ExceptT () IO) ()]
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
            => Lens' SynLam syn -> StateT SynLam (ExceptT () IO) ()
          delegate p = zoom p $ do
            react (asyncReact . over p) oldLayout inputEvent
        selection <- gets synSelection
        case selection of
          Nothing          -> throwError ()
          Just SelLamArg   -> delegate synLamArg
          Just SelLamExpr1 -> delegate synLamExpr1
          Just SelLamExpr2 -> delegate synLamExpr2

instance SyntaxBlank SynLam where
  blank = return SynLam
    { _synLamArg   = SynArg (SynText "_" 1 False)
    , _synLamExpr1 = SynHollow
    , _synLamExpr2 = SynHollow
    , _synLamSel   = Nothing
    }


---        Pi       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynPi where

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
  ) => SyntaxReact n m (CollageDraw' n m) SynPi where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT SynPi (ExceptT () IO) ()]
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
            => Lens' SynPi syn -> StateT SynPi (ExceptT () IO) ()
          delegate p = zoom p $ do
            react (asyncReact . over p) oldLayout inputEvent
        selection <- gets synSelection
        case selection of
          Nothing         -> throwError ()
          Just SelPiArg   -> delegate synPiArg
          Just SelPiExpr1 -> delegate synPiExpr1
          Just SelPiExpr2 -> delegate synPiExpr2

instance SyntaxBlank SynPi where
  blank = return SynPi
    { _synPiArg   = SynArg (SynText "_" 1 False)
    , _synPiExpr1 = SynHollow
    , _synPiExpr2 = SynHollow
    , _synPiSel   = Nothing
    }


---       App       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynApp where

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
  ) => SyntaxReact n m (CollageDraw' n m) SynApp where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT SynApp (ExceptT () IO) ()]
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
            => Lens' SynApp syn -> StateT SynApp (ExceptT () IO) ()
          delegate p = zoom p $ do
            react (asyncReact . over p) oldLayout inputEvent
        selection <- gets synSelection
        case selection of
          Nothing          -> throwError ()
          Just SelAppExpr1 -> delegate synAppExpr1
          Just SelAppExpr2 -> delegate synAppExpr2

instance SyntaxBlank SynApp where
  blank = return SynApp
    { _synAppExpr1 = SynHollow
    , _synAppExpr2 = SynHollow
    , _synAppSel   = Nothing
    }


---      Const      ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynConst where

  layout _ = \case
    SynConstStar -> punct "★"
    SynConstBox  -> punct "□"

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) SynConst where

---       Var       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) () SynInt where

  layout () (SynInt n) = (text . Text.pack . show) n

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynVar where

  layout lctx v =
    [ layout lctx (v ^. synVarName)
    , if (v ^. synVarIndex . _SynInt) > 0
      then
        [ text "@"
        , layout () (v ^. synVarIndex)
        ] & horizontal
      else
        mempty
    ] & horizontal

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) SynVar where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT SynVar (ExceptT () IO) ()]
      handlers =
        [ handleShiftUp
        , handleShiftDown
        , handleRedirect
        ]
      handleShiftUp =
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'U' keyCode
            -> synVarIndex . _SynInt += 1
          _ -> throwError ()
      handleShiftDown =
        case inputEvent of
          KeyPress [Shift] keyCode | keyLetter 'D' keyCode
            -> synVarIndex . _SynInt %= max 0 . subtract 1
          _ -> throwError ()
      handleRedirect = zoom synVarName $ do
        react (asyncReact . over synVarName) oldLayout inputEvent

instance SyntaxBlank SynVar where
  blank = return $ SynVar (SynText "" 0 True) (SynInt 0)

---      Embed      ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynEmbed where

  layout _ _ = text "Embed"

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxReact n m (CollageDraw' n m) SynEmbed where

---       Expr      ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) LayoutCtx SynExpr where

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
  ) => SyntaxReact n m (CollageDraw' n m) SynExpr where

  react asyncReact oldLayout inputEvent =
    let
      delegate
        :: SyntaxReact n m (CollageDraw' n m) syn
        => Prism' SynExpr syn -> StateT SynExpr (ExceptT () IO) ()
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
      handlers :: [ExceptT () IO SynExpr]
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

instance
  ( n ~ Int, m ~ Int
  , SyntaxLayout n m (CollageDraw' n m) lctx syn
  ) => SyntaxLayout n m (CollageDraw' n m) lctx (SynHole syn) where

  layout lctx = \case
    SynHollow    -> punct "⦿"
    SynSolid syn -> layout lctx syn

  draw _ = draw'

instance
  ( n ~ Int, m ~ Int
  , SyntaxReact n m (CollageDraw' n m) syn
  ) => SyntaxReact n m (CollageDraw' n m) (SynHole syn) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (SynHole syn) (ExceptT () IO) ()]
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

instance SyntaxBlank (SynHole syn) where
  blank = return SynHollow


---       Syn       ---
---    instances    ---

instance
  ( n ~ Int, m ~ Int
  ) => SyntaxLayout n m (CollageDraw' n m) (Extents n m) (Syn n m) where

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
  ) => SyntaxReact n m (CollageDraw' n m) (Syn n m) where

  react asyncReact oldLayout inputEvent = asum handlers
    where
      handlers :: [StateT (Syn n m) (ExceptT () IO) ()]
      handlers =
        [ handlePointerMotion
        , handleButtonPress
        , handleCtrl_h
        , handle_u
        , handleCtrl_r
        , handleRedirectExpr
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
        synUndo %= (expr:)
        synRedo .= []

updateExprPath :: Path -> SynHole SynExpr -> SynHole SynExpr
updateExprPath path = \case
  SynHollow -> SynHollow
  SynSolid e -> SynSolid $ case e of
    SynExprLam   a -> SynExprLam (updateLamPath path a)
    SynExprPi    a -> SynExprPi  (updatePiPath  path a)
    SynExprApp   a -> SynExprApp (updateAppPath path a)
    SynExprConst _ -> e
    SynExprVar   _ -> e
    SynExprEmbed _ -> e

updateLamPath :: Path -> SynLam -> SynLam
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

updatePiPath :: Path -> SynPi -> SynPi
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

updateAppPath :: Path -> SynApp -> SynApp
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

instance (n ~ Int, m ~ Int) => SyntaxBlank (Syn n m) where
  blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    expr <- synImportExpr <$> case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> M.I.load e
    return $ Syn (SynSolid expr) (0, 0) False [] []


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
