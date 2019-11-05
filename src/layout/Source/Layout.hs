-- | A DSL for layout definitions.
module Source.Layout where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text)
import Sdam.Core

-- | Is a precedence border needed?
newtype PrecBorder = PrecBorder Bool

instance Semigroup PrecBorder where
  PrecBorder a <> PrecBorder b = PrecBorder (a || b)

instance Monoid PrecBorder where
  mempty = PrecBorder False

-- | Layouts not enclosed by a precedence border.
newtype PrecUnenclosed = PrecUnenclosed (HashSet TyName)

instance Semigroup PrecUnenclosed where
  PrecUnenclosed a <> PrecUnenclosed b =
    PrecUnenclosed (HashSet.union a b)

instance Monoid PrecUnenclosed where
  mempty = PrecUnenclosed HashSet.empty

addUnenclosed :: TyName -> PrecUnenclosed -> PrecUnenclosed
addUnenclosed tyName (PrecUnenclosed s) =
  PrecUnenclosed (HashSet.insert tyName s)

guardUnenclosed :: PrecBorder -> PrecUnenclosed -> PrecUnenclosed
guardUnenclosed (PrecBorder True) = const mempty
guardUnenclosed (PrecBorder False) = id

newtype PrecPredicate
  = PrecPredicate {appPrecPredicate :: PrecUnenclosed -> PrecBorder}

precAllow :: HashSet TyName -> PrecPredicate
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

class (IsString a, Semigroup a) => Layout a where

  vsep :: a -> a -> a

  field :: FieldName -> PrecPredicate -> Text -> a

infixr 1 `vsep`

noPrec :: PrecPredicate
noPrec = PrecPredicate (const (PrecBorder True))

newtype ALayoutFn = ALayoutFn (forall a. Layout a => a)

instance IsString ALayoutFn where
  fromString s = ALayoutFn (fromString s)

instance Semigroup ALayoutFn where
  ALayoutFn a <> ALayoutFn b =
    ALayoutFn (a <> b)

instance Layout ALayoutFn where

  ALayoutFn a `vsep` ALayoutFn b =
    ALayoutFn (a `vsep` b)

  field fieldName precPredicate placeholder =
    ALayoutFn (field fieldName precPredicate placeholder)
