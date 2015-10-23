{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Source.Language.Morte
    ( State
    ) where

import Control.Lens
import Control.Monad
import Data.Char (toLower)
import Data.Functor.Compose
import Data.Singletons
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Biapplicative
import Data.String (fromString)
import Data.Function
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text.Lazy as Text.Lazy
import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

import Source.Syntax
import Source.Draw
import Source.Style
import Source.Input
import qualified Source.Input.KeyCode as KeyCode
import Source.Language.Morte.Node

data State n m = State
  { _stateExpr :: NodeExpr
  , _statePath :: Discard PathExpr
  , _statePointer :: Offset n m }
makeLenses ''State

instance (n ~ Int, m ~ Int) => Syntax n m (CollageDraw' n m) (State n m) where
  blank = blank'
  layout = layout'
  draw _ = draw'
  react = react'

blank' :: IO (State Int Int)
blank' = do
  let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
  _stateExpr <- view noded <$> case M.P.exprFromText et of
    Left  _ -> return $ M.Const M.Star
    Right e -> M.I.load e
  let
    _statePath = Discard pHere
    _statePointer = (0, 0)
  return State{..}

getExcess :: Integral n => n -> n -> (n, n)
getExcess vacant actual =
  let
    excess = max 0 (vacant - actual)
    excess1 = excess `quot` 2
    excess2 = excess - excess1
  in (excess1, excess2)

center
  :: ( Integral n
     , Integral m
     , HasExtents n m draw
     , DrawPhantom n m draw
     ) => Extents n m -> Op1 (Collage n m draw)
center (vacantWidth, vacantHeight) collage =
  let
    (width, height) = getExtents collage
    (excessWidth1,  excessWidth2)  = getExcess vacantWidth  width
    (excessHeight1, excessHeight2) = getExcess vacantHeight height
  in collage & pad (excessWidth1, excessHeight1) (excessWidth2, excessHeight2)

align
  :: ( Integral n
     , Integral m
     , HasExtents n m draw
     , DrawPhantom n m draw
     ) => (Op2 n, Op2 m)
       -> (Extents n m -> Offset n m)
       -> Op2 (Collage n m draw)
align adjust move c1 c2 =
  let vacant = adjust <<*>> getExtents c1 <<*>> getExtents c2
  in overlay move (center vacant c1) (center vacant c2)

verticalCenter
  :: ( Integral n
     , Integral m
     , HasExtents n m draw
     , DrawPhantom n m draw
     ) => OpN (Collage n m draw)
verticalCenter = foldr (align (max, \_ _ -> 0) (_1 .~ 0)) mempty

horizontalCenter
  :: ( Integral n
     , Integral m
     , HasExtents n m draw
     , DrawPhantom n m draw
     ) => OpN (Collage n m draw)
horizontalCenter = foldr (align (\_ _ -> 0, max) (_2 .~ 0)) mempty

line
  :: ( Num n
     , Num m
     , Ord n
     , Ord m
     , HasExtents n m draw
     , DrawPhantom n m draw
     , DrawRectangle n m draw
     ) => Color -> n -> Collage n m draw
line color w = pure (drawRectangle (w, 1) Nothing (Just color))

pad
  :: ( Num n
     , Num m
     , Ord n
     , Ord m
     , HasExtents n m draw
     , DrawPhantom n m draw
     ) => Offset n m -> Offset n m -> Op1 (Collage n m draw)
pad o1 o2 = offset o1 . extend o2

data Draw' n m
  = Draw' {- z-index -} (Draw n m)
  | ActiveZone (Extents n m) (Discard PathExpr)

active :: (Num n, Ord n, Num m, Ord m) => PathExpr p -> Op1 (CollageDraw' n m)
active p c = pure (ActiveZone (getExtents c) (Discard p)) `mappend` c

activate
  :: forall n m r
   . (Ord n, Num n, Ord m, Num m)
  => (Offset n m -> Extents n m -> Discard PathExpr -> r)
  -> Offset n m
  -> CollageDraw' n m
  -> Maybe r
activate f o = getLast . foldMap (Last . uncurry check) . view _Collage
  where
    within :: (Ord a, Num a) => a -> a -> a -> Bool
    within a zoneOffset zoneExtents
       = a >  zoneOffset
      && a < (zoneOffset + zoneExtents)
    check :: Offset n m -> Draw' n m -> Maybe r
    check o' d = do
      ActiveZone e p <- Just d
      guard
        $ uncurry (&&)
        $ biliftA3 within within o o' e
      Just (f o' e p)

hover
  :: forall n m
   . (Ord n, Num n, Ord m, Num m)
  => Op1 (CollageDraw' n m)
  -> Offset n m
  -> Op1 (CollageDraw' n m)
hover f o c = c <> maybe mempty id (activate obj o c)
  where
    obj o e _ = offset o (f (phantom e))

instance DrawPhantom n m (Draw' n m) where
  drawPhantom e = Draw' (drawPhantom e)

instance DrawRectangle n m (Draw' n m) where
  drawRectangle e o b = Draw' (drawRectangle e o b)

instance (Integral n, Integral m) => DrawText n m (Draw' n m) where
  drawText f t = Draw' (drawText f t)

instance HasExtents n m (Draw' n m) where
  getExtents = \case
    Draw' d -> getExtents d
    ActiveZone e _ -> e

type CollageDraw' n m = Collage n m (Draw' n m)

draw' :: (Num n, Num m) => CollageDraw' n m -> CollageDraw n m
draw' c = c >>= \case
  Draw' d -> pure d
  _       -> mempty

layout' :: Extents Int Int -> State Int Int -> IO (CollageDraw' Int Int)
layout' viewport state = do
  return
    $ hover (outline light1) (state ^. statePointer)
    $ background dark1
    $ center viewport
    $ layoutExpr (join pad (5, 5)) pHere (state ^. stateExpr)
  where
    dark1 = RGB 0.2 0.2 0.2
    dark2 = RGB 0.3 0.3 0.3
    dark3 = RGB 0.25 0.25 0.25
    light1 = RGB 0.7 0.7 0.7
    font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal
    text = textline font
    punct = textline (font { fontColor = light1 })

    sel :: forall q . PathExpr q -> CollageDraw' Int Int -> CollageDraw' Int Int
    sel path
      | current = active path . outline dark2 . background dark3
      | otherwise = active path
      where
        current = Discard path == state ^. statePath

    layoutExpr
      :: Op1 (CollageDraw' Int Int)
      -> PathExpr ('LabelSum 'Expr)
      -> NodeExpr
      -> CollageDraw' Int Int
    layoutExpr hook path
      = sel path
      . hook
      . onExpr
          layoutConst
          layoutVar
          (layoutLam $ path -@- pExprLam)
          (layoutPi  $ path -@- pExprPi)
          (layoutApp $ path -@- pExprApp)
          layoutEmbed

    layoutConst :: NodeConst -> CollageDraw' Int Int
    layoutConst (End c) = case c of
      M.Star -> punct "★"
      M.Box  -> punct "□"

    layoutVar :: NodeVar -> CollageDraw' Int Int
    layoutVar (End (M.V txt n)) = text (Text.Lazy.toStrict txt <> i)
      where
        -- TODO: subscript
        i = if n == 0 then "" else "@" <> fromString (show n)

    layoutApp
      :: PathExpr ('LabelProduct 'App)
      -> NodeApp
      -> CollageDraw' Int Int
    layoutApp path = withProduct $ \get ->
        [ layoutExpr
            (join pad (5, 5))
            (path -@- pAppExpr1)
            (get SAppExpr1)
        , join pad (5, 5) (layoutExpr
            (outline dark2 . join pad (5, 5))
            (path -@- pAppExpr2)
            (get SAppExpr2))
        ] & horizontalCenter

    layoutLam
      :: PathExpr ('LabelProduct 'Lam)
      -> NodeLam
      -> CollageDraw' Int Int
    layoutLam path = withProduct $ \get ->
      let
        maxWidth = (max `on` fst.getExtents) header body
        header =
          [ extend (4, 0) (punct "λ")
          , [ layoutArg
                (join pad (4, 0))
                (path -@- pLamArg)
                (get SLamArg)
            , join pad (4, 0) (punct ":")
            , layoutExpr (join pad (4, 0)) (path -@- pLamExpr1) (get SLamExpr1)
            ] & horizontal
          ] & horizontal
        body = layoutExpr id (path -@- pLamExpr2) (get SLamExpr2)
      in
        [ header
        , join pad (0, 4) (line light1 maxWidth)
        , body
        ] & vertical

    layoutPi
      :: PathExpr ('LabelProduct 'Pi)
      -> NodePi
      -> CollageDraw' Int Int
    layoutPi path = withProduct $ \get ->
      let
        maxWidth = (max `on` fst.getExtents) header body
        header =
          [ extend (4, 0) (punct "Π")
          , [ layoutArg
                (join pad (4, 0))
                (path -@- pPiArg)
                (get SPiArg)
            , join pad (4, 0) (punct ":")
            , layoutExpr (join pad (4, 0)) (path -@- pPiExpr1) (get SPiExpr1)
            ] & horizontal
          ] & horizontal
        body = layoutExpr id (path -@- pPiExpr2) (get SPiExpr2)
      in
        [ header
        , join pad (0, 4) (line light1 maxWidth)
        , body
        ] & vertical

    layoutEmbed :: NodeEmbed -> CollageDraw' Int Int
    layoutEmbed (End r) = case r of {}

    layoutArg
      :: Op1 (CollageDraw' Int Int)
      -> PathExpr ('LabelEnd 'Arg)
      -> NodeArg
      -> CollageDraw' Int Int
    layoutArg hook path = sel path . hook . text . unEnd

react'
  :: ((State Int Int -> State Int Int) -> IO ())
  -> CollageDraw' Int Int
  -> InputEvent Int Int
  -> State Int Int
  -> IO (Maybe (State Int Int))
react' _asyncReact layout inputEvent state

  | KeyPress _ keyCode <- inputEvent
  , keyCode == KeyCode.ArrowUp || keyLetter 'k' keyCode
  = return
  $ updatePath
  $ withDiscard pathUp (state ^. statePath)

  | KeyPress _ keyCode <- inputEvent
  , keyCode == KeyCode.ArrowDown || keyLetter 'j' keyCode
  = return
  $ updatePath
  $ withDiscard (pathChild (state ^. stateExpr)) (state ^. statePath)

  | KeyPress mod keyCode <- inputEvent
  , keyCode == KeyCode.ArrowLeft || keyLetter 'h' keyCode
  = return
  $ updatePath
  $ withDiscard
      (if Shift `elem` mod then pathNeighbourL else pathSiblingL)
      (state ^. statePath)

  | KeyPress mod keyCode <- inputEvent
  , keyCode == KeyCode.ArrowRight || keyLetter 'l' keyCode
  = return
  $ updatePath
  $ withDiscard
      (if Shift `elem` mod then pathNeighbourR else pathSiblingR)
      (state ^. statePath)

  | PointerMotion x y <- inputEvent
  = return . Just
  $ state & statePointer .~ (x, y)

  | ButtonPress <- inputEvent
  = return
  $ updatePath
  $ activate (\_ _ p -> p) (state ^. statePointer) layout

  | otherwise
  = return Nothing

  where
    updatePath :: Maybe (Discard PathExpr) -> Maybe (State Int Int)
    updatePath mpath = set statePath <$> mpath ?? state

    keyLetter c keyCode = fmap toLower (keyChar keyCode) == Just c

pathNormalize :: Op1 (Discard (Path p))
pathNormalize (Discard path) = fromMaybe (Discard path) (pathSumUp path)

pathUp :: Path p q -> Maybe (Discard (Path p))
pathUp path = pathNormalize <$> pathProductUp path

pathUps :: Path p q -> NonEmpty (Discard (Path p))
pathUps path = Discard path :| maybe [] (toList . withDiscard pathUps) (pathUp path)

pathSumUp :: Path p q -> Maybe (Discard (Path p))
pathSumUp = \case
  (r :@> Here) -> Just (Discard (withSingI (sRelationSumLabel r) Here))
  (r :@> p1) -> withDiscard (\p1' -> Discard (r :@> p1')) <$> pathSumUp p1
  (r :@- p1) -> withDiscard (\p1' -> Discard (r :@- p1')) <$> pathSumUp p1
  Here -> Nothing

pathProductUp :: Path p q -> Maybe (Discard (Path p))
pathProductUp = \case
  (r :@- Here) -> Just (Discard (withSingI (sRelationProductLabel r) Here))
  (r :@- p1) -> withDiscard (\p1' -> Discard (r :@- p1')) <$> pathProductUp p1
  (r :@> p1) -> withDiscard (\p1' -> Discard (r :@> p1')) <$> pathProductUp p1
  Here -> Nothing

pathSumDown :: Path p q -> [Discard (Path p)]
pathSumDown path = case pathTarget path of
  SLabelSum l -> case l of
    SExpr ->
      [ Discard (path -@- pExprConst)
      , Discard (path -@- pExprVar)
      , Discard (path -@- pExprLam)
      , Discard (path -@- pExprPi)
      , Discard (path -@- pExprApp)
      , Discard (path -@- pExprEmbed) ]
  _ -> []

pathProductDown :: Path p q -> [Discard (Path p)]
pathProductDown path = case pathTarget path of
  SLabelProduct l -> case l of
    SLam ->
      [ Discard (path -@- pLamExpr2)
      , Discard (path -@- pLamExpr1)
      , Discard (path -@- pLamArg) ]
    SPi ->
      [ Discard (path -@- pPiExpr2)
      , Discard (path -@- pPiExpr1)
      , Discard (path -@- pPiArg) ]
    SApp ->
      [ Discard (path -@- pAppExpr2)
      , Discard (path -@- pAppExpr1) ]
  _ -> []

pathChild :: Node p -> Path p q -> Maybe (Discard (Path p))
pathChild node path = pathChildren node path ^? _head

pathChildren :: Node p -> Path p q -> [Discard (Path p)]
pathChildren node path = do
  Discard path'  <- pathSumDown path
  Discard path'' <- pathProductDown path'
  guard $ notNullOf (atPath path'') node
  return (Discard path'')

data Direction = L | R

newtype CyclicStep = CyclicStep Bool

pathSibling' :: Direction -> Path p q -> Compose Maybe ((,) CyclicStep) (Discard (Path p))
pathSibling' direction = \case
  (r :@- Here) -> Compose . Just $ case (direction, r) of
    (L, SLamArg)   -> (CyclicStep True,  Discard pLamExpr2)
    (R, SLamArg)   -> (CyclicStep False, Discard pLamExpr1)
    (L, SLamExpr1) -> (CyclicStep False, Discard pLamArg)
    (R, SLamExpr1) -> (CyclicStep False, Discard pLamExpr2)
    (L, SLamExpr2) -> (CyclicStep False, Discard pLamExpr1)
    (R, SLamExpr2) -> (CyclicStep True,  Discard pLamArg)
    (L, SPiArg)    -> (CyclicStep True,  Discard pPiExpr2)
    (R, SPiArg)    -> (CyclicStep False, Discard pPiExpr1)
    (L, SPiExpr1)  -> (CyclicStep False, Discard pPiArg)
    (R, SPiExpr1)  -> (CyclicStep False, Discard pPiExpr2)
    (L, SPiExpr2)  -> (CyclicStep False, Discard pPiExpr1)
    (R, SPiExpr2)  -> (CyclicStep True,  Discard pPiArg)
    (L, SAppExpr1) -> (CyclicStep True,  Discard pAppExpr2)
    (R, SAppExpr1) -> (CyclicStep False, Discard pAppExpr2)
    (L, SAppExpr2) -> (CyclicStep False, Discard pAppExpr1)
    (R, SAppExpr2) -> (CyclicStep True,  Discard pAppExpr1)
  (r :@- p1) -> withDiscard (\p1' -> Discard (r :@- p1')) <$> pathSibling' direction p1
  (r :@> p1) -> withDiscard (\p1' -> Discard (r :@> p1')) <$> pathSibling' direction p1
  Here -> Compose Nothing

pathSiblingL', pathSiblingR' :: Path p q -> Maybe (CyclicStep, Discard (Path p))
pathSiblingL' = getCompose . pathSibling' L
pathSiblingR' = getCompose . pathSibling' R

nonCyclic :: (CyclicStep, a) -> Maybe a
nonCyclic (CyclicStep cyclic, a) = pure a <* guard (not cyclic)

pathSiblingL, pathSiblingR :: Path p q -> Maybe (Discard (Path p))
pathSiblingL path = snd <$> pathSiblingL' path
pathSiblingR path = snd <$> pathSiblingR' path

pathNeighbourL :: Path p q -> Maybe (Discard (Path p))
pathNeighbourL path =
  (getFirst . foldMap First)
  (withDiscard (nonCyclic <=< pathSiblingL') <$> pathUps path)

pathNeighbourR :: Path p q -> Maybe (Discard (Path p))
pathNeighbourR path =
  (getFirst . foldMap First)
  (withDiscard (nonCyclic <=< pathSiblingR') <$> pathUps path)
