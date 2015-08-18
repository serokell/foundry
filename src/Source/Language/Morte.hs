{-# LANGUAGE TemplateHaskell #-}
module Source.Language.Morte
    ( State
    ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Bool
import Data.Monoid
import Data.String (fromString)
import Control.Lens

import Source.Syntax
import Source.Style
import Source.Layout

import qualified Morte.Core as M
import qualified Morte.Parser as M.P
import qualified Morte.Import as M.I

data Hole = Blank | Path M.Path

data State = State
    { _stateExpr :: M.Expr Hole
    }

makeLenses ''State

instance Syntax State where

 blank = do
    let et = "#http://sigil.place/tutorial/morte/1.2/even"
    e <- case M.P.exprFromText et of
      Left  _ -> return $ M.Const M.Star
      Right e -> fmap M.absurd <$> M.I.load e
    return $ State e

 layout viewport state = do
    return $ background dark1
           $ centerContainer viewport
           $ vertical [layoutExpr (view stateExpr state)]
  where
    dark1 = RGB 0.2 0.2 0.2
    dark2 = RGB 0.3 0.3 0.3
    light1 = RGB 0.7 0.7 0.7
    font = Font "Ubuntu" 12 (RGB 1 1 1) FontWeightNormal
    text = layoutText font
    punct = layoutText (font { fontColor = light1 })

    line :: Color -> Int -> Layout
    line color w
        = background color
        $ extend (Extents w 1)
        $ horizontal []

    frame :: Layout -> Layout
    frame = pad 5 5 5 5 . border dark2 . pad 5 5 5 5

    layoutExpr :: M.Expr Hole -> Layout
    layoutExpr = \case
        M.Const c -> layoutConst c
        M.Var   x -> layoutVar   x
        M.Lam x _A  b -> layoutLam (Text.Lazy.toStrict x) _A  b
        M.Pi  x _A _B -> layoutPi  (Text.Lazy.toStrict x) _A _B
        M.App f a -> layoutApp f a
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
    
    layoutCorner :: Text -> Text -> M.Expr Hole -> M.Expr Hole -> Layout
    layoutCorner sym x _A b = vertical
        [ headerBox
        , pad 0 0 4 4 $ line light1 (width headerBox `max` width bodyBox)
        , bodyBox
        ]
      where
        headerBox = horizontal
            [ pad 0 4 0 0 (punct sym)
            , horizontal
                $ map (pad 4 4 0 0)
                $ [text x, punct ":", layoutExpr _A]
            ]
        bodyBox = layoutExpr b

    layoutLam = layoutCorner "λ"
    layoutPi  = layoutCorner "Π"

    layoutApp :: M.Expr Hole -> M.Expr Hole -> Layout
    layoutApp f a = (center . horizontal)
        [ layoutExpr f
        , frame (layoutExpr a)
        ]

 react _ _ _ = return Nothing
