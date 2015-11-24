{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Source.Language.Morte
    ( State
    ) where

import Control.Lens
import Control.Monad
import Data.Char (toLower)
import Data.Functor.Compose
import Data.Foldable
import Data.Monoid
import Data.Biapplicative
import Data.String (fromString)
import Data.Function
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
  { _stateExpr :: Node
  , _statePath :: Path
  , _statePointer :: Offset n m
  }

makeLenses ''State

instance (n ~ Int, m ~ Int) => Syntax n m (CollageDraw' n m) (State n m) where
  blank = blank'
  layout = layout'
  draw _ = draw'
  react = react'

blank' :: IO (State Int Int)
blank' = do
  let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
  _stateExpr <- nodeImportExpr <$> case M.P.exprFromText et of
    Left  _ -> return $ M.Const M.Star
    Right e -> M.I.load e
  let
    _statePath = mempty
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
  | ActiveZone (Extents n m) Path

active :: (Num n, Ord n, Num m, Ord m) => Path -> Op1 (CollageDraw' n m)
active p c = pure (ActiveZone (getExtents c) p) `mappend` c

activate
  :: forall n m r
   . (Ord n, Num n, Ord m, Num m)
  => (Offset n m -> Extents n m -> Path -> r)
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
    $ layoutExpr (join pad (5, 5)) mempty (state ^. stateExpr)
  where
    dark1 = RGB 0.2 0.2 0.2
    dark2 = RGB 0.3 0.3 0.3
    dark3 = RGB 0.25 0.25 0.25
    light1 = RGB 0.7 0.7 0.7
    font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal
    text = textline font
    punct = textline (font { fontColor = light1 })

    sel :: Path -> CollageDraw' Int Int -> CollageDraw' Int Int
    sel path
      | current = active path . outline dark2 . background dark3
      | otherwise = active path
      where
        current = path == state ^. statePath

    layoutExpr
      :: Op1 (CollageDraw' Int Int)
      -> Path
      -> Node
      -> CollageDraw' Int Int
    layoutExpr hook path
      = sel path
      . hook
      . onExpr
          layoutHole
          layoutConst
          layoutVar
          (layoutLam path)
          (layoutPi path)
          (layoutApp path)
          layoutEmbed

    layoutHole :: () -> CollageDraw' Int Int
    layoutHole () = punct "_"

    layoutConst :: Node -> CollageDraw' Int Int
    layoutConst = onConst layoutHole $ \case
      M.Star -> punct "★"
      M.Box  -> punct "□"

    layoutVar :: Node -> CollageDraw' Int Int
    layoutVar = onVar layoutHole $ \(M.V txt n) ->
      let
        -- TODO: subscript
        i = if n == 0 then "" else "@" <> fromString (show n)
      in text (Text.Lazy.toStrict txt <> i)

    layoutLam :: Path -> Node -> CollageDraw' Int Int
    layoutLam path = onLam $ \arg expr1 expr2 ->
      let
        maxWidth = (max `on` fst.getExtents) header body
        header =
          [ extend (4, 0) (punct "λ")
          , [ layoutArg
                (join pad (4, 0))
                (path <> rp LamArg)
                arg
            , join pad (4, 0) (punct ":")
            , layoutExpr (join pad (4, 0)) (path <> rp LamExpr1) expr1
            ] & horizontal
          ] & horizontal
        body = layoutExpr id (path <> rp LamExpr2) expr2
      in
        [ header
        , join pad (0, 4) (line light1 maxWidth)
        , body
        ] & vertical

    layoutPi :: Path -> Node -> CollageDraw' Int Int
    layoutPi path = onPi $ \arg expr1 expr2 ->
      let
        maxWidth = (max `on` fst.getExtents) header body
        header =
          [ extend (4, 0) (punct "Π")
          , [ layoutArg
                (join pad (4, 0))
                (path <> rp PiArg)
                arg
            , join pad (4, 0) (punct ":")
            , layoutExpr (join pad (4, 0)) (path <> rp PiExpr1) expr1
            ] & horizontal
          ] & horizontal
        body = layoutExpr id (path <> rp PiExpr2) expr2
      in
        [ header
        , join pad (0, 4) (line light1 maxWidth)
        , body
        ] & vertical

    layoutApp :: Path -> Node -> CollageDraw' Int Int
    layoutApp path = onApp $ \expr1 expr2 ->
        [ layoutExpr
            (join pad (5, 5))
            (path <> rp AppExpr1)
            expr1
        , join pad (5, 5) (layoutExpr
            (outline dark2 . join pad (5, 5))
            (path <> rp AppExpr2)
            expr2)
        ] & horizontalCenter

    layoutEmbed :: Node -> CollageDraw' Int Int
    layoutEmbed _ = mempty

    layoutArg
      :: Op1 (CollageDraw' Int Int)
      -> Path
      -> Node
      -> CollageDraw' Int Int
    layoutArg hook path = sel path . hook . onArg layoutHole text

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
  $ pathUp (state ^. statePath)

  | KeyPress _ keyCode <- inputEvent
  , keyCode == KeyCode.ArrowDown || keyLetter 'j' keyCode
  = return
  $ updatePath
  $ pathChild (state ^. stateExpr) (state ^. statePath)

  | KeyPress mod keyCode <- inputEvent
  , keyCode == KeyCode.ArrowLeft || keyLetter 'h' keyCode
  = return
  $ updatePath
  $ (if Control `elem` mod then pathNeighbourL else pathSiblingL)
    (state ^. statePath)

  | KeyPress mod keyCode <- inputEvent
  , keyCode == KeyCode.ArrowRight || keyLetter 'l' keyCode
  = return
  $ updatePath
  $ (if Control `elem` mod then pathNeighbourR else pathSiblingR)
    (state ^. statePath)

  | KeyPress _ keyCode <- inputEvent
  , keyCode == KeyCode.Delete || keyLetter 'x' keyCode
  = return
  $ state & failover
      (stateExpr . atPath (state ^. statePath))
      (\_ -> mkHole ())

  | KeyPress _ keyCode <- inputEvent
  , keyLetter 'L' keyCode
  = return
  $ updateHole Lam (\_ -> let h = mkHole() in mkLam h h h)

  | KeyPress _ keyCode <- inputEvent
  , keyLetter 'P' keyCode
  = return
  $ updateHole Pi (\_ -> let h = mkHole() in mkPi h h h)

  | KeyPress _ keyCode <- inputEvent
  , keyLetter 'A' keyCode
  = return
  $ updateHole App (\_ -> let h = mkHole() in mkApp h h)

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
    updatePath :: Maybe Path -> Maybe (State Int Int)
    updatePath mpath = set statePath <$> mpath ?? state

    updateHole :: Label -> (() -> Node) -> Maybe (State Int Int)
    updateHole l onHole = do
      case state ^? statePath . _Path . _last of
        Nothing -> guard (l `elem` exprLabels)
        Just  r -> guard (l `elem` slaveLabels r)
      state & (stateExpr . atPath (state ^. statePath))
              (fillHole onHole)

    keyLetter c keyCode = keyChar keyCode == Just c

pathUp :: Path -> Maybe Path
pathUp = preview (_Path . _Snoc . _1 . from _Path)

pathUps :: Path -> NonEmpty Path
pathUps path = path :| maybe [] (toList . pathUps) (pathUp path)

pathChild :: Node -> Path -> Maybe Path
pathChild node path = pathChildren node path ^? _head

pathChildren :: Node -> Path -> [Path]
pathChildren node path = do
  l <- case path ^? _Path . _last of
    Nothing -> node ^.. nodeLabel
    Just r -> slaveLabels r
  r <- slaveRelations l
  let path' = path <> rp r
  guard $ notNullOf (atPath path') node
  return path'

slaveRelations :: Label -> [Relation]
slaveRelations = \case
  Lam -> [LamExpr2, LamExpr1, LamArg]
  Pi  -> [PiExpr2, PiExpr1, PiArg]
  App -> [AppExpr1, AppExpr2]
  _   -> []

data Direction = L | R

newtype CyclicStep = CyclicStep Bool

pathSibling' :: Direction -> Path -> Compose Maybe ((,) CyclicStep) Path
pathSibling' direction path = case path ^? _Path . _Snoc of
  Nothing -> Compose Nothing
  Just (rs, r) ->
    let
      p = case (direction, r) of
        (L, LamArg)   -> (CyclicStep True,  LamExpr2)
        (R, LamArg)   -> (CyclicStep False, LamExpr1)
        (L, LamExpr1) -> (CyclicStep False, LamArg)
        (R, LamExpr1) -> (CyclicStep False, LamExpr2)
        (L, LamExpr2) -> (CyclicStep False, LamExpr1)
        (R, LamExpr2) -> (CyclicStep True,  LamArg)
        (L, PiArg)    -> (CyclicStep True,  PiExpr2)
        (R, PiArg)    -> (CyclicStep False, PiExpr1)
        (L, PiExpr1)  -> (CyclicStep False, PiArg)
        (R, PiExpr1)  -> (CyclicStep False, PiExpr2)
        (L, PiExpr2)  -> (CyclicStep False, PiExpr1)
        (R, PiExpr2)  -> (CyclicStep True,  PiArg)
        (L, AppExpr1) -> (CyclicStep True,  AppExpr2)
        (R, AppExpr1) -> (CyclicStep False, AppExpr2)
        (L, AppExpr2) -> (CyclicStep False, AppExpr1)
        (R, AppExpr2) -> (CyclicStep True,  AppExpr1)
    in review _Path . snoc rs <$> Compose (Just p)

pathSiblingL', pathSiblingR' :: Path -> Maybe (CyclicStep, Path)
pathSiblingL' = getCompose . pathSibling' L
pathSiblingR' = getCompose . pathSibling' R

nonCyclic :: (CyclicStep, a) -> Maybe a
nonCyclic (CyclicStep cyclic, a) = pure a <* guard (not cyclic)

pathSiblingL, pathSiblingR :: Path -> Maybe Path
pathSiblingL path = snd <$> pathSiblingL' path
pathSiblingR path = snd <$> pathSiblingR' path

pathNeighbourL :: Path -> Maybe Path
pathNeighbourL path =
  (getFirst . foldMap First)
  ((nonCyclic <=< pathSiblingL') <$> pathUps path)

pathNeighbourR :: Path -> Maybe Path
pathNeighbourR path =
  (getFirst . foldMap First)
  ((nonCyclic <=< pathSiblingR') <$> pathUps path)
