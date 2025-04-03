{- |

An injection is a function that never maps distinct elements of the domain to
the same element of the codomain. For example, @(\\x -> x + 1)@ is an injection,
but @(\\x -> min x 0)@ is not.

Injections can be used to construct nested structures from singleton elements.

-}

module Source.Layout.Inj (Inj(..), Inj1, nothing) where

import Data.Void
import Numeric.Natural
import Data.Functor.Identity

-- | Inject @p@ into @a@.
--
-- By convention, the instances of @Inj@ never match on @p@ and always match on
-- @a@. This guarantees that the users will not encounter overlapping instances.
class Inj p a where
  -- | Inject @p@ into @a@.
  inj :: p -> a

  default inj :: (p ~ a) => p -> a
  inj = \x -> x

-- | Throws 'Underflow'.
instance Integral p => Inj p Natural where
  inj = fromIntegral

-- | Injective only if the number is representable as 'Double'.
instance Real p => Inj p Double where
  inj = realToFrac

data Decision_Wrap

data Decision_Map

type family DecideIdentity p where
  DecideIdentity (Identity p) = Decision_Map
  DecideIdentity p = Decision_Wrap

class d ~ DecideIdentity p => InjIdentity d p a where
  injIdentity :: p -> Identity a

instance InjIdentity (DecideIdentity p) p a => Inj p (Identity a) where
  inj = injIdentity

instance
    (DecideIdentity p ~ Decision_Wrap, Inj p a) =>
    InjIdentity Decision_Wrap p a
  where
    injIdentity = pure . inj

instance
    (DecideIdentity p ~ Decision_Map, p ~ Identity p', Inj p' a) =>
    InjIdentity Decision_Map p a
  where
    injIdentity = fmap inj

type family DecideMaybe p where
  DecideMaybe (Maybe p) = Decision_Map
  DecideMaybe p = Decision_Wrap

class d ~ DecideMaybe p => InjMaybe d p a where
  injMaybe :: p -> Maybe a

instance InjMaybe (DecideMaybe p) p a => Inj p (Maybe a) where
  inj = injMaybe

instance
    (DecideMaybe p ~ Decision_Wrap, Inj p a) =>
    InjMaybe Decision_Wrap p a
  where
    injMaybe = pure . inj

instance
    (DecideMaybe p ~ Decision_Map, p ~ Maybe p', Inj p' a) =>
    InjMaybe Decision_Map p a
  where
    injMaybe = fmap inj

type family DecideFn p where
  DecideFn (r -> p) = Decision_Map
  DecideFn p = Decision_Wrap

class d ~ DecideFn p => InjFn d p r a where
  injFn :: p -> r -> a

instance InjFn (DecideFn p) p r a => Inj p (r -> a) where
  inj = injFn

instance
    (DecideFn p ~ Decision_Wrap, Inj p a) =>
    InjFn Decision_Wrap p s a
  where
    injFn = pure . inj

instance
    (DecideFn p ~ Decision_Map, p ~ (r -> p'), Inj p' a) =>
    InjFn Decision_Map p r a
  where
    injFn = fmap inj

nothing :: Inj (Maybe Void) a => a
nothing = inj (Nothing @Void)

instance {-# OVERLAPPING #-} Inj Void Natural where
  inj = absurd

class Inj t (f t) => Inj1 f t

instance Inj t (f t) => Inj1 f t
