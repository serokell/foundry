{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
module Source.Syntax
    ( SyntaxSelection(..)
    , SyntaxLayout(..)
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
    , LayoutCtx(..)
    , lctxPath
    , lctxViewport
    , Collage
    , Draw
    , PathSegment(..)
    , Path
    , fromPathSegment
    , Fields
    , Idx(..)
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Source.Input
import Source.Draw
import Source.Path

newtype Viewport = Viewport Extents

makePrisms ''Viewport

data ReactCtx syn = ReactCtx
  { _rctxAsyncReact :: ((syn -> syn) -> IO ())
  , _rctxLastLayout :: Collage Draw
  , _rctxInputEvent :: InputEvent Int
  }

makeLenses ''ReactCtx

data LayoutCtx = LayoutCtx
  { _lctxPath     :: Path
  , _lctxViewport :: Viewport
  }

makeLenses ''LayoutCtx

type React syn =
  ReaderT (ReactCtx syn) (StateT syn (MaybeT IO)) ()

type Subreact syn =
  ReaderT (ReactCtx syn) (MaybeT IO) syn

reactRedirect
  :: SyntaxReact syn
  => Traversal' syn' syn
  -> React syn'
reactRedirect l = do
  let liftReactCtx = over rctxAsyncReact (. over l)
  guard =<< gets (notNullOf l)
  mapReaderT (zoom l) $ withReaderT liftReactCtx react

subreactRedirect
  :: SyntaxReact syn
  => Prism' syn' syn
  -> Subreact syn'
subreactRedirect l = do
  let liftReactCtx = over rctxAsyncReact (. over l)
  review l <$> withReaderT liftReactCtx subreact

subreactToReact :: Subreact syn -> React syn
subreactToReact = put <=< mapReaderT lift

class SyntaxSelection syn where
  selectionPath :: syn -> Path

class SyntaxLayout syn where
  layout :: syn -> Reader LayoutCtx (Collage Draw)

class SyntaxReact syn where
  react :: React syn
  react = mzero

  subreact :: Subreact syn
  subreact = mzero

class SyntaxBlank syn where
  blank :: IO syn
