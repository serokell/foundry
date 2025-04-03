module Sdam.Syn
  ( Syn (..),
    Token(..),
    SynShape,
    synShape,
    synFields,
    SynReconstructException(..),
    synReconstruct,
    synTryReconstruct,
    parseSynShape,
    flattenSynShape,
  )
where

import Data.Hashable as Hashable
import Data.String
import Data.Sequence as Seq
import Data.Foldable as Foldable
import Control.Monad.Trans.State
import GHC.Generics (Generic)
import Control.Exception

{- |

Syntactic construct with slots.

'Syn' is parametrized by the type of its fields. In the trivial case, we can
take the fixpoint of 'Syn' to have nodes that are made of nodes:

  newtype AST = AST (Syn AST)

However, we may also use this for extension:

  data Editable =
      Node UUID (Syn Editable)
    | Hole

-}

newtype Syn a = Syn { synTokens :: Seq (Token a) }
  deriving stock (Eq, Ord, Show, Generic)
  deriving stock (Foldable, Functor, Traversable)

instance Hashable a => Hashable (Syn a) where
  hashWithSalt salt = hashWithSalt salt . Foldable.toList . synTokens

type SynShape = Syn ()

synShape :: Syn a -> SynShape
synShape = fmap (const ())

synFields :: Syn a -> [a]
synFields = toList

data SynReconstructException
  = SynReconstructNotEnoughFields
  | SynReconstructTooManyFields
  deriving Show

instance Exception SynReconstructException

synReconstruct :: SynShape -> [a] -> Syn a
synReconstruct shape fields = either throw id (synTryReconstruct shape fields)

synTryReconstruct :: SynShape -> [a] -> Either SynReconstructException (Syn a)
synTryReconstruct shape fields = do
  let
    uncons (x : xs) = Right (x, xs)
    uncons [] = Left SynReconstructNotEnoughFields
  (r, rest) <- runStateT (traverse (\() -> StateT uncons) shape) fields
  case rest of
    [] -> Right r
    _ : _ -> Left SynReconstructTooManyFields

flattenSynShape :: SynShape -> String
flattenSynShape = detokenize . Foldable.toList . synTokens

parseSynShape :: String -> SynShape
parseSynShape = Syn . Seq.fromList . tokenize

instance s ~ () => IsString (Syn s) where
  fromString = parseSynShape

data Token a
  = TokenChar Char
  | TokenNode a
  deriving stock (Eq, Ord, Show, Generic)
  deriving stock (Foldable, Functor, Traversable)

instance Hashable a => Hashable (Token a)

-- tokenize . detokenize = id
-- detokenize . tokenize = id

tokenize :: String -> [Token ()]
tokenize [] = []
tokenize ('_' : cs) = TokenNode () : tokenize cs
tokenize ('\\' : c : cs) =
  if needsEscape c
    then TokenChar c : tokenize cs
    else TokenChar '\\' : TokenChar c : tokenize cs
tokenize (c : cs) = TokenChar c : tokenize cs

detokenize :: [Token ()] -> String
detokenize [] = []
detokenize (TokenNode () : ts) = '_' : detokenize ts
detokenize (TokenChar c : ts) =
  if needsEscape c
    then '\\' : c : detokenize ts
    else c : detokenize ts

needsEscape :: Char -> Bool
needsEscape c =
  case c of
    '\n' -> True
    '_' -> True
    '\\' -> True
    _ -> False
