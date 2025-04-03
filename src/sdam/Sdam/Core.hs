module Sdam.Core
  (
    -- * Syn
    module Sdam.Syn,

    -- * Paths
    Path(..),
    emptyPath,
    consPath,
    unconsPath,
    PathSegment(..),
    Index,
    intToIndex,
    indexToInt,
    PathBuilder(..),
    mkPathBuilder,
    buildPath,
    PathTrie(..),
    pathTrieLookup
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict as HashMap
import Control.Exception (ArithException(Underflow), throw)
import GHC.Generics (Generic)

import Sdam.Syn

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

newtype Path = Path [PathSegment]
  deriving newtype (Eq, Show)

emptyPath :: Path
emptyPath = Path []

consPath :: PathSegment -> Path -> Path
consPath ps (Path p) = Path (ps:p)

unconsPath :: Path -> Maybe (PathSegment, Path)
unconsPath (Path p) =
  case p of
    [] -> Nothing
    ps : p' -> Just (ps, Path p')

{-

Note that 'PathSegment' is qualified by a 'SynShape':

* This guarantees that there is no clash between fields with the same name
  across different types (that is, each type has its own namespace for fields).

* This safeguards against duck typing. We wouldn't want code that abstracts over
  values by what fields /names/ they have: abstraction should be over meaning,
  not over strings.

-}
data PathSegment = PathSegment SynShape Index
  deriving stock (Eq, Show, Generic)

instance Hashable PathSegment

-- Invariant: non-negative.
newtype Index = Index Int
  deriving newtype (Eq, Show, Hashable)

intToIndex :: Int -> Index
intToIndex i =
  if i < 0
    then throw Underflow
    else Index i

indexToInt :: Index -> Int
indexToInt (Index i) = i

newtype PathBuilder = PathBuilder (Path -> Path)

instance Semigroup PathBuilder where
  PathBuilder f <> PathBuilder g = PathBuilder (f . g)

instance Monoid PathBuilder where
  mempty = PathBuilder id

mkPathBuilder :: PathSegment -> PathBuilder
mkPathBuilder ps = PathBuilder (consPath ps)

buildPath :: PathBuilder -> Path
buildPath (PathBuilder pb) = pb emptyPath

data PathTrie a =
  PathTrie
    { pathTrieRoot :: a,
      pathTrieChildren :: HashMap PathSegment (PathTrie a)
    }

instance Semigroup a => Semigroup (PathTrie a) where
  PathTrie r1 c1 <> PathTrie r2 c2 =
    PathTrie (r1 <> r2) (HashMap.unionWith (<>) c1 c2)

instance Monoid a => Monoid (PathTrie a) where
  mempty = PathTrie mempty HashMap.empty

pathTrieLookup :: Monoid a => PathSegment -> PathTrie a -> PathTrie a
pathTrieLookup pathSegment pathTrie =
  HashMap.lookupDefault mempty pathSegment (pathTrieChildren pathTrie)
