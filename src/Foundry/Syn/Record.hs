module Foundry.Syn.Record where

import Data.Kind (Type)
import Data.Foldable
import Control.Monad.Reader
import Control.Lens
import Type.Reflection

import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Vinyl hiding (Dict)
import Data.Constraint

import Source.Syntax
import Source.Draw
import qualified Source.Input.KeyCode as KeyCode

import Foundry.Syn.Common

type family FieldTypes (sel :: Type) :: [Type]

newtype SynRecField k (sel :: k) = SynRecField (FieldTypes k !! FromEnum sel)

deriving instance UndoEq (FieldTypes k !! FromEnum sel) => UndoEq (SynRecField k sel)

makePrisms ''SynRecField

type SynRec sel =
  Rec (SynRecField sel) (EnumFromTo MinBound MaxBound)

data SynRecord sel = SynRecord
  { _synRec :: SynRec sel
  , _synRecSel :: Integer
  , _synRecSelSelf :: Bool
  }

makeLenses ''SynRecord

instance UndoEq (SynRec sel) => UndoEq (SynRecord sel) where
  undoEq a1 a2 = undoEq (a1 ^. synRec) (a2 ^. synRec)

synField
  :: ((r :: sel) ∈ EnumFromTo MinBound MaxBound)
  => Sing r
  -> Lens' (SynRecord sel) (FieldTypes sel !! FromEnum r)
synField s = synRec . rlens s . _SynRecField

-- Selection-related classes

selOrder :: (Enum s, Bounded s) => [s]
selOrder = [minBound .. maxBound]

lookupNext :: Eq s => [s] -> s -> Maybe s
lookupNext ss s = lookup s (ss `zip` tail ss)

selRevOrder :: (Enum s, Bounded s) => [s]
selRevOrder = reverse selOrder

selNext, selPrev :: (Enum s, Bounded s, Eq s) => s -> Maybe s
selNext = lookupNext selOrder
selPrev = lookupNext selRevOrder

class SelLayout la s | s -> la where
  selLayoutC ::
    forall rp a.
    Sing (a :: s) ->
    Dict
      (SyntaxReact rp la (FieldTypes s !! FromEnum a),
       a ∈ EnumFromTo MinBound MaxBound)
  selLayoutHook :: s' -/ Draw Path => s -> Collage s' -> Collage s'

handleArrowUp :: SynSelection syn sel => React rp la syn
handleArrowUp = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
  False <- use synSelectionSelf
  synSelectionSelf .= True

handleArrowDown :: SynSelection syn sel => React rp la syn
handleArrowDown = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
  True <- use synSelectionSelf
  synSelectionSelf .= False

handleArrowLeft
  :: (SynSelection syn sel, Enum sel, Bounded sel, Eq sel)
  => React rp la syn
handleArrowLeft = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (selPrev selection)
  synSelection .= selection'

handleArrowRight
  :: (SynSelection syn sel, Enum sel, Bounded sel, Eq sel)
  => React rp la syn
handleArrowRight = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (selNext selection)
  synSelection .= selection'

handleArrows
  :: (SynSelection syn sel, Enum sel, Bounded sel, Eq sel)
  => React rp la syn
handleArrows = asum @[]
  [handleArrowUp, handleArrowDown, handleArrowLeft, handleArrowRight]

instance (SingKind sel, Demote sel ~ sel, Enum sel)
      => SynSelfSelected (SynRecord sel)

instance (SingKind sel, Demote sel ~ sel, Enum sel)
      => SynSelection (SynRecord sel) sel where
  synSelection = synRecSel . iso (toEnum . fromIntegral) (toInteger . fromEnum)
  synSelectionSelf = synRecSelSelf

recHandleSelRedirect ::
  forall t rp la.
     (Demote t ~ t, SingKind t, Enum t, SelLayout la t) =>
  React rp la (SynRecord t)
recHandleSelRedirect = do
  False <- use synSelectionSelf
  SomeSing selection <- uses synRecSel (toSing . toEnum . fromIntegral)
  case selLayoutC @la @_ @rp selection of
    Dict -> reactRedirect (synField selection)

selLayout ::
  forall t (a :: t) la.
     (Demote t ~ t, SelLayout la t, Enum t,
      (a ∈ EnumFromTo MinBound MaxBound),
      SingKind t, SyntaxLayout Path LayoutCtx (FieldTypes t !! FromEnum a),
      SynSelfSelected (FieldTypes t !! FromEnum a), Typeable t, Eq t)
  => Sing a
  -> SynRecord t
  -> forall s. (s -/ Draw Path)
  => Reader LayoutCtx (Collage s)
selLayout ssel syn = do
  let
    sel' = fromSing ssel
    sub = view (synField ssel) syn
    appendSelection
      = (lctxSelected &&~ (view synSelection syn == sel'))
      . (lctxSelected &&~ (synSelfSelected syn == False))
      . (lctxPath %~ (`snoc` PathSegment typeRep sel'))
    enforceSelfSelection
      = lctxSelected &&~ synSelfSelected sub
  local appendSelection $ do
    a <- layout sub
    reader $ \lctx ->
       sel (enforceSelfSelection lctx)
     $ selLayoutHook sel' a
