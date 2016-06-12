module Foundry.Syn.Record where

import Data.Kind (Type)
import Data.Foldable
import Control.Monad.Reader
import Control.Lens
import Data.Dynamic

import Data.Singletons.Prelude
import Data.Vinyl

import qualified Data.Singletons.TH as Sing
import qualified Language.Haskell.TH as TH

import Source.Syntax
import Source.Collage
import qualified Source.Input.KeyCode as KeyCode

import Foundry.Syn.Common

type family FieldType (sel :: k) :: Type

newtype SynRecField (sel :: k) = SynRecField (FieldType sel)

deriving instance UndoEq (FieldType sel) => UndoEq (SynRecField sel)

makePrisms ''SynRecField

type SynRec sel =
  Rec (SynRecField :: sel -> Type) (EnumFromTo MinBound MaxBound)

data SynRecord sel = SynRecord
  { _synRec :: SynRec sel
  , _synRecSel :: SomeSing ('KProxy :: KProxy sel)
  , _synRecSelSelf :: Bool
  }

-- makeLenses fails for some reason.

synRec :: Lens' (SynRecord sel) (SynRec sel)
synRec = lens _synRec (\s b -> s {_synRec = b})

synRecSel :: Lens' (SynRecord sel) (SomeSing ('KProxy :: KProxy sel))
synRecSel = lens _synRecSel (\s b -> s {_synRecSel = b})

synRecSelSelf :: Lens' (SynRecord sel) Bool
synRecSelSelf = lens _synRecSelSelf (\s b -> s {_synRecSelSelf = b})

synField
  :: ((r :: sel) ∈ EnumFromTo MinBound MaxBound)
  => Sing r
  -> Lens' (SynRecord sel) (FieldType r)
synField s = synRec . rlens s . _SynRecField

someSingIso
  :: SingKind ('KProxy :: KProxy k)
  => Iso' (SomeSing  ('KProxy :: KProxy k))
          (DemoteRep ('KProxy :: KProxy k))
someSingIso = iso (\(SomeSing ss) -> fromSing ss) toSing

-- Selection-related classes

class Eq s => Sel s where
  selOrder :: [s]
  default selOrder :: (Enum s, Bounded s) => [s]
  selOrder = [minBound .. maxBound]

lookupNext :: Eq s => [s] -> s -> Maybe s
lookupNext ss s = lookup s (ss `zip` tail ss)

selRevOrder :: Sel s => [s]
selRevOrder = reverse selOrder

selNext, selPrev :: Sel s => s -> Maybe s
selNext = lookupNext selOrder
selPrev = lookupNext selRevOrder

class SelLayout s where
  selLayoutHook :: s -> Op1 (CollageDraw' Int)

handleArrowUp :: SynSelection syn sel => React n rp la syn
handleArrowUp = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowUp 'k'
  False <- use synSelectionSelf
  synSelectionSelf .= True

handleArrowDown :: SynSelection syn sel => React n rp la syn
handleArrowDown = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowDown 'j'
  True <- use synSelectionSelf
  synSelectionSelf .= False

handleArrowLeft
  :: (SynSelection syn sel, Sel sel)
  => React n rp la syn
handleArrowLeft = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (selPrev selection)
  synSelection .= selection'

handleArrowRight
  :: (SynSelection syn sel, Sel sel)
  => React n rp la syn
handleArrowRight = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (selNext selection)
  synSelection .= selection'

handleArrows
  :: (SynSelection syn sel, Sel sel)
  => React n rp la syn
handleArrows = asum
  [handleArrowUp, handleArrowDown, handleArrowLeft, handleArrowRight]

instance ( kproxy ~ ('KProxy :: KProxy sel)
         , SingKind kproxy
         , demoteRep ~ (DemoteRep kproxy) )
      => SynSelfSelected (SynRecord sel)
instance ( kproxy ~ ('KProxy :: KProxy sel)
         , SingKind kproxy
         , demoteRep ~ (DemoteRep kproxy) )
      => SynSelection (SynRecord sel) demoteRep where
  synSelection = synRecSel . someSingIso
  synSelectionSelf = synRecSelSelf

instance UndoEq (Rec SynRecField '[]) where
  undoEq RNil RNil = True

instance (UndoEq (SynRecField a), UndoEq (Rec SynRecField as))
      => UndoEq (Rec SynRecField (a ': as)) where
  undoEq (a1 :& as1) (a2 :& as2) = undoEq a1 a2 && undoEq as1 as2

instance ( kproxy ~ ('KProxy :: KProxy sel)
         , UndoEq (SynRec sel) )
      => UndoEq (SynRecord sel) where
  undoEq a1 a2 = undoEq (a1 ^. synRec) (a2 ^. synRec)

recHandleSelRedirect :: TH.Name -> TH.ExpQ
recHandleSelRedirect selTypeName =
  [e| do False <- use synSelectionSelf
         SomeSing selection <- use synRecSel
         $(Sing.sCases selTypeName [e|selection|]
             [e|reactRedirect (synField selection)|])
   |]

selLayout ssel syn = do
  let
    sel' = fromSing ssel
    sub = view (synField ssel) syn
    appendSelection
      = (lctxSelected &&~ (view synSelection syn == sel'))
      . (lctxSelected &&~ (synSelfSelected syn == False))
      . (lctxPath %~ (`snoc` toDyn sel'))
    enforceSelfSelection
      = lctxSelected &&~ synSelfSelected sub
  local appendSelection $ do
    a <- layout sub
    reader $ \lctx ->
       sel (enforceSelfSelection lctx)
     $ selLayoutHook sel' a
