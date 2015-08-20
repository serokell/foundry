module Control.Lens.Discard
    ( Discard(..)
    , D'LensLike
    , D'LensLike'
    , D'Traversal
    , D'Traversal'
    , D'Endo
    , d'preview
    , d'over
    ) where

import Data.Monoid
import Control.Lens

data Discard t where
    Discard :: t a -> Discard t

type D'LensLike f s t a b = (forall e . a e -> f (b e)) -> s -> f t
type D'LensLike' f s a = D'LensLike f s s a a

type D'Traversal s t a b = forall f . Applicative f => D'LensLike f s t a b
type D'Traversal' s a = D'Traversal s s a a

type D'Endo t = forall a . t a -> t a

d'preview :: D'LensLike' (Const (First (Discard a))) s a -> s -> Maybe (Discard a)
d'preview l = getFirst . getConst . l (Const . First . Just . Discard)

d'over :: D'LensLike' Identity s a -> D'Endo a -> s -> s
d'over l h = runIdentity . l (Identity . h)

