module Source.Plugin.Precedence
  ( PrecBorder (PrecBorder),
    PrecUnenclosed (PrecUnenclosed),
    addUnenclosed,
    guardUnenclosed,
    PrecPredicate (PrecPredicate, appPrecPredicate),
    precAllow,
    precAllowAll,
    noPrec,
  )
where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Sdam.Core

-- | Is a precedence border needed?
newtype PrecBorder = PrecBorder Bool

instance Semigroup PrecBorder where
  PrecBorder a <> PrecBorder b = PrecBorder (a || b)

instance Monoid PrecBorder where
  mempty = PrecBorder False

-- | Layouts not enclosed by a precedence border.
newtype PrecUnenclosed = PrecUnenclosed (HashSet SynShape)

instance Semigroup PrecUnenclosed where
  PrecUnenclosed a <> PrecUnenclosed b =
    PrecUnenclosed (HashSet.union a b)

instance Monoid PrecUnenclosed where
  mempty = PrecUnenclosed HashSet.empty

addUnenclosed :: SynShape -> PrecUnenclosed -> PrecUnenclosed
addUnenclosed shape (PrecUnenclosed s) =
  PrecUnenclosed (HashSet.insert shape s)

guardUnenclosed :: PrecBorder -> PrecUnenclosed -> PrecUnenclosed
guardUnenclosed (PrecBorder True) = const mempty
guardUnenclosed (PrecBorder False) = id

newtype PrecPredicate
  = PrecPredicate {appPrecPredicate :: PrecUnenclosed -> PrecBorder}

precAllow :: HashSet SynShape -> PrecPredicate
precAllow allowed =
  PrecPredicate $ \(PrecUnenclosed unenclosed) ->
    PrecBorder $
      -- Need a border unless all of unenclosed layouts are allowed.
      not (unenclosed `hashSet_isSubsetOf` allowed)

precAllowAll :: PrecPredicate
precAllowAll = PrecPredicate (const (PrecBorder False))

hashSet_isSubsetOf :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
hashSet_isSubsetOf sub sup =
  all (\k -> HashSet.member k sup) sub

noPrec :: PrecPredicate
noPrec = PrecPredicate (const (PrecBorder True))
