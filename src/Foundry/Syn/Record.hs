module Foundry.Syn.Record where

import Data.Foldable
import Control.Lens

import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe

import Data.Vinyl
import Data.Vinyl.TypeLevel

import Source.Syntax
import Source.Collage
import qualified Source.Input.KeyCode as KeyCode

import Foundry.Syn.Common

type family FieldType (sel :: k) :: *

type FieldTypes s ss = FromJust (Lookup s
  (Zip (EnumFromTo MinBound MaxBound) ss))

newtype SynRecField (sel :: k) = SynRecField (FieldType sel)
  deriving (UndoEq)

makePrisms ''SynRecField

type SynRec (kproxy :: KProxy sel) =
  Rec (SynRecField :: sel -> *) (EnumFromTo MinBound MaxBound)

data SynRecord (kproxy :: KProxy sel) = SynRecord
  { _synRec :: SynRec ('KProxy :: KProxy sel)
  , _synRecSel :: SomeSing ('KProxy :: KProxy sel)
  , _synRecSelSelf :: Bool
  }

-- makeLenses fails for some reason.

synRec
  :: Lens'
      (SynRecord ('KProxy :: KProxy sel))
      (SynRec    ('KProxy :: KProxy sel))
synRec = lens _synRec (\s b -> s {_synRec = b})

synRecSel
  :: Lens'
       (SynRecord ('KProxy :: KProxy sel))
       (SomeSing  ('KProxy :: KProxy sel))
synRecSel = lens _synRecSel (\s b -> s {_synRecSel = b})

synRecSelSelf :: Lens' (SynRecord kproxy) Bool
synRecSelSelf = lens _synRecSelSelf (\s b -> s {_synRecSelSelf = b})

synField
  :: RElem
      (r :: sel)
      (EnumFromTo MinBound MaxBound)
      (RIndex r (EnumFromTo MinBound MaxBound))
  => Sing r
  -> Lens' (SynRecord ('KProxy :: KProxy sel)) (FieldType r)
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
      => SynSelfSelected (SynRecord kproxy)
instance ( kproxy ~ ('KProxy :: KProxy sel)
         , SingKind kproxy
         , demoteRep ~ (DemoteRep kproxy) )
      => SynSelection (SynRecord kproxy) demoteRep where
  synSelection = synRecSel . someSingIso
  synSelectionSelf = synRecSelSelf
