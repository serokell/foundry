module Sdam.Validator
  ( -- * Schema
    Schema (..),
    TyUnion,
    mkTyUnion,

    -- * Validation
    ValidationError (..),
    ValidationResult,
    ValidationValue (..),
    validate,
  )
where

import Data.Foldable as Foldable
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.List as List
import GHC.Generics (Generic)
import Sdam.Core
import Prelude hiding (seq)

data Schema
  = Schema
      { schemaShapes :: HashSet SynShape,
        schemaRoot :: TyUnion,
        schemaSeqGuard :: Char
      }

newtype TyUnion = TyUnion (HashMap SynShape [TyUnion])

instance Semigroup TyUnion where
  TyUnion u1 <> TyUnion u2 = TyUnion (HashMap.unionWith tyDescUnion u1 u2)
    where
      tyDescUnion :: [TyUnion] -> [TyUnion] -> [TyUnion]
      tyDescUnion = List.zipWith (<>)

instance Monoid TyUnion where
  mempty = TyUnion HashMap.empty

mkTyUnion :: SynShape -> [TyUnion] -> TyUnion
mkTyUnion shape fields =
  let syn = synReconstruct shape fields -- does arity validation
   in TyUnion (HashMap.singleton (synShape syn) (synFields syn))

isSeq :: Char -> Syn a -> Bool
isSeq seqGuard syn =
  case Foldable.toList (synTokens syn) of
    (TokenChar c : ts) | c == seqGuard, all isTokenNode ts -> True
    _ -> False
  where
    isTokenNode :: Token a -> Bool
    isTokenNode (TokenChar _) = False
    isTokenNode (TokenNode _) = True

data ValidationError
  = UnknownShape SynShape
  | TypeMismatch SynShape (HashSet SynShape)
  deriving (Eq, Show, Generic)

instance Hashable ValidationError

type ValidationResult = PathTrie (HashSet ValidationError)

validationError :: ValidationError -> ValidationResult
validationError e = mempty {pathTrieRoot = HashSet.singleton e}

data ValidationValue
  = ValidationValue (Syn ValidationValue)
  | SkipValidation
  deriving stock (Show)

validate :: Schema -> ValidationValue -> ValidationResult
validate schema = vValue schemaRoot
  where
    Schema {schemaShapes, schemaRoot, schemaSeqGuard} = schema
    vValue ::
      TyUnion ->
      ValidationValue ->
      ValidationResult
    vValue _ SkipValidation = mempty
    vValue tyU (ValidationValue syn) =
      let shape = synShape syn
          fields = synFields syn
       in if isSeq schemaSeqGuard syn
            then vSeq shape tyU fields
            else vShape shape tyU (\fieldTys -> vRec shape fieldTys fields)
    vShape ::
      SynShape ->
      TyUnion ->
      ([TyUnion] -> ValidationResult) ->
      ValidationResult
    vShape shape (TyUnion u) cont =
      case HashSet.member shape schemaShapes of
        False -> validationError (UnknownShape shape)
        True ->
          case HashMap.lookup shape u of
            Nothing -> validationError (TypeMismatch shape (HashMap.keysSet u))
            Just fieldTys -> cont fieldTys
    vRec ::
      SynShape ->
      [TyUnion] ->
      [ValidationValue] ->
      ValidationResult
    vRec shape fieldTys fields =
      let mkPathSegment i = PathSegment shape (intToIndex i)
          vField i ty fld = (mkPathSegment i, vValue ty fld)
          pathTrieRoot = HashSet.empty
          pathTrieChildren =
            HashMap.fromList $
              List.zipWith3
                vField
                [0 ..]
                fieldTys
                fields
       in PathTrie {pathTrieRoot, pathTrieChildren}
    vSeq ::
      SynShape ->
      TyUnion ->
      [ValidationValue] ->
      ValidationResult
    vSeq shape ty fields =
      let mkPathSegment i = PathSegment shape (intToIndex i)
          vField i fld = (mkPathSegment i, vValue ty fld)
          pathTrieRoot = HashSet.empty
          pathTrieChildren =
            HashMap.fromList $
              List.zipWith
                vField
                [0 ..]
                fields
       in PathTrie {pathTrieRoot, pathTrieChildren}
