{-# LANGUAGE TemplateHaskell #-}
module Source.Language.Morte
    ( State
    ) where

import Control.Monad
import Data.Foldable
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as Text.Lazy
import Data.Bool
import Data.Maybe
import Data.Monoid
import Data.String (fromString)
import Control.Lens

import Source.Syntax
import Source.Input
import Source.Style
import Source.Layout
import qualified Source.Input.KeyCode as KeyCode

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

type Path = Seq Int

data PP a = PPText Text | PPExpr (M.Expr a)

-- TODO: a prism
atPath :: Path -> M.Expr a -> Maybe (PP a)
atPath = atPath' . toList
  where
    atPath' [] e = Just (PPExpr e)
    atPath' (0:steps) (M.Lam _ _  b) = atPath' steps b
    atPath' ((-1):steps) (M.Lam _ _A _) = atPath' steps _A
    atPath' [-2] (M.Lam x _   _) = Just (PPText (Text.Lazy.toStrict x))
    atPath' (0:steps) (M.Pi _ _  _B) = atPath' steps _B
    atPath' ((-1):steps) (M.Pi _ _A  _) = atPath' steps _A
    atPath' [-2] (M.Pi x _   _) = Just (PPText (Text.Lazy.toStrict x))
    atPath' (0:steps) (M.App f _) = atPath' steps f
    atPath' (1:steps) (M.App _ x) = atPath' steps x
    atPath' _ _ = Nothing


data Hole = Blank | Path M.Path

data State = State
    { _stateExpr :: M.Expr Hole
    , _statePath :: Path
    }

makeLenses ''State

instance Syntax State where

 blank = do
    let et = "λ(x : ∀(Nat : *) → ∀(Succ : Nat → Nat) → ∀(Zero : Nat) → Nat) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(x : ∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) → x (∀(Bool : *) → ∀(True : Bool) → ∀(False : Bool) → Bool) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → False) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)) (λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True)"
    e <- case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> fmap M.absurd <$> M.I.load e
    return $ State e Seq.empty

 layout viewport state = do
    return $ background dark1
           $ centerContainer viewport
           $ vertical [layoutExpr (pad 5 5 5 5) Seq.empty (view stateExpr state)]
  where
    dark1 = RGB 0.2 0.2 0.2
    dark2 = RGB 0.3 0.3 0.3
    dark3 = RGB 0.25 0.25 0.25
    light1 = RGB 0.7 0.7 0.7
    font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal
    text = layoutText font
    punct = layoutText (font { fontColor = light1 })

    sel :: Path -> Layout -> Layout
    sel path
        | current = border dark2 . background dark3
        | otherwise = id
      where current = path == state ^. statePath

    line :: Color -> Int -> Layout
    line color w
        = background color
        $ extend (Extents w 1)
        $ horizontal []

    layoutExpr :: (Layout -> Layout) -> Path -> M.Expr Hole -> Layout
    layoutExpr hook path = sel path . hook . \case
        M.Const c -> layoutConst c
        M.Var   x -> layoutVar   x
        M.Lam x _A  b -> layoutLam path (Text.Lazy.toStrict x) _A  b
        M.Pi  x _A _B -> layoutPi  path (Text.Lazy.toStrict x) _A _B
        M.App f a -> layoutApp path f a
        _ -> text "Can't render"

    layoutConst :: M.Const -> Layout
    layoutConst = \case
        M.Star -> punct "★"
        M.Box -> punct "□"

    layoutVar :: M.Var -> Layout
    layoutVar (M.V txt n) = text (Text.Lazy.toStrict txt <> i)
      where
        -- TODO: subscript
        i = if n == 0 then "" else "@" <> fromString (show n)

    layoutCorner :: Text -> Path -> Text -> M.Expr Hole -> M.Expr Hole -> Layout
    layoutCorner sym path x _A b = vertical
        [ headerBox
        , pad 0 0 4 4 $ line light1 (width headerBox `max` width bodyBox)
        , bodyBox
        ]
      where
        headerBox = horizontal
            [ pad 0 4 0 0 (punct sym)
            , horizontal
              [ sel (path |> (-2)) (pad 4 4 0 0 (text x))
              , pad 4 4 0 0 (punct ":")
              , layoutExpr (pad 4 4 0 0) (path |> (-1)) _A
              ]
            ]
        bodyBox = layoutExpr id (path |> 0) b

    layoutLam = layoutCorner "λ"
    layoutPi  = layoutCorner "Π"

    layoutApp :: Path -> M.Expr Hole -> M.Expr Hole -> Layout
    layoutApp path f a = (center . horizontal)
        [ layoutExpr (pad 5 5 5 5) (path |> 0) f
        , (pad 5 5 5 5 . border dark2)
          (layoutExpr (pad 5 5 5 5) (path |> 1) a)
        ]

 react _ (KeyPress modifiers keyCode) state
  | keyCode == KeyCode.ArrowLeft || keyChar keyCode == Just 'h'
  = return . Just
  $ updatePath
  $ pathNeighbourL (state ^. stateExpr) (state ^. statePath)

  | keyCode == KeyCode.ArrowRight || keyChar keyCode == Just 'l'
  = return . Just
  $ updatePath
  $ pathNeighbourR (state ^. stateExpr) (state ^. statePath)

  | keyCode == KeyCode.ArrowUp || keyChar keyCode == Just 'k'
  = return . Just
  $ updatePath
  $ pathUp (state ^. statePath)

  | keyCode == KeyCode.ArrowDown || keyChar keyCode == Just 'j'
  = return . Just
  $ updatePath
  $ pathChild (state ^. stateExpr) (state ^. statePath)

  | otherwise
  = return Nothing

  where

    updatePath :: Maybe Path -> State
    updatePath mpath = fromMaybe id (set statePath <$> mpath) state

    pathUp :: Path -> Maybe Path
    pathUp path = path ^? _init

    pathChild :: M.Expr a -> Path -> Maybe Path
    pathChild = pathNthChild 0

    pathNthChild :: Int -> M.Expr a -> Path -> Maybe Path
    pathNthChild n expr path = do
        PPExpr subexpr <- atPath path expr
        case subexpr of
            M.App _ _ -> do
                guard (n >= 0 && n <= 1)
                return (path |> n)
            M.Lam _ _ _ -> do
                guard (n >= (-2) && n <= 0)
                return (path |> n)
            M.Pi _ _ _ -> do
                guard (n >= (-2) && n <= 0)
                return (path |> n)
            _ -> Nothing

    pathChildN :: Path -> Maybe Int
    pathChildN path = path ^? _last

    pathSibling :: (Int -> Int) -> M.Expr a -> Path -> Maybe Path
    pathSibling f expr path = do
        n <- pathChildN path
        pathUp path >>= pathNthChild (f n) expr

    pathNeighbour :: (Int -> Int) -> M.Expr a -> Path -> Maybe Path
    pathNeighbour f term path = asum (pathSibling f term <$> pathUps)
      where pathUps = (reverse.toList) (Seq.inits path)

    pathNeighbourL, pathNeighbourR :: M.Expr a -> Path -> Maybe Path
    pathNeighbourL = pathNeighbour (subtract 1)
    pathNeighbourR = pathNeighbour (+ 1)
