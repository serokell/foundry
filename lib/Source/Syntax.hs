{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
module Source.Syntax
    ( SyntaxLayout(..)
    , SyntaxBlank(..)
    , SyntaxReact(..)
    , Viewport(..)
    , _Viewport
    , ReactCtx(..)
    , React
    , Subreact
    , reactRedirect
    , subreactRedirect
    , subreactToReact
    , rctxAsyncReact
    , rctxLastLayout
    , rctxInputEvent
    , CollageDraw
    , CollageBuilderDraw
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Source.Input
import Source.Draw

type CollageDraw n a = Collage n (Draw a)

newtype Viewport n = Viewport (Extents n)

makePrisms ''Viewport

data ReactCtx n rp la syn = ReactCtx
  { _rctxAsyncReact :: ((syn -> syn) -> IO ())
  , _rctxLastLayout :: CollageDraw n la
  , _rctxInputEvent :: InputEvent n
  , _rctxPayload    :: rp
  }

makeLenses ''ReactCtx

type React n rp la syn =
  ReaderT (ReactCtx n rp la syn) (StateT syn (MaybeT IO)) ()

type Subreact n rp la syn =
  ReaderT (ReactCtx n rp la syn) (MaybeT IO) syn

reactRedirect
  :: SyntaxReact n rp la syn
  => Traversal' syn' syn
  -> React n rp la syn'
reactRedirect l = do
  let liftReactCtx = over rctxAsyncReact (. over l)
  guard =<< gets (notNullOf l)
  mapReaderT (zoom l) $ withReaderT liftReactCtx react

subreactRedirect
  :: SyntaxReact n rp la syn
  => Prism' syn' syn
  -> Subreact n rp la syn'
subreactRedirect l = do
  let liftReactCtx = over rctxAsyncReact (. over l)
  review l <$> withReaderT liftReactCtx subreact

subreactToReact :: Subreact n rp la syn -> React n rp la syn
subreactToReact = put <=< mapReaderT lift

class SyntaxLayout n la lctx syn | syn -> la, syn -> lctx where
  layout :: syn -> Reader lctx (CollageBuilderDraw n la)

class SyntaxReact n rp la syn | syn -> la where
  react :: React n rp la syn
  react = mzero

  subreact :: Subreact n rp la syn
  subreact = mzero

class SyntaxBlank syn where
  blank :: IO syn
