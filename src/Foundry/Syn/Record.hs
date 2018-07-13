{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS -fno-warn-unticked-promoted-constructors #-}

module Foundry.Syn.Record where

import Data.Kind (Type, Constraint)
import Data.Foldable
import Data.Functor.Product
import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import GHC.TypeLits hiding (type (*))
import Type.Reflection
import qualified Data.Functor.Unwrapped as F

import Source.Syntax
import Source.Draw
import qualified Source.Input.KeyCode as KeyCode

import Foundry.Syn.Common

type family NN n where
  NN 0 = Z
  NN n = S (NN (n - 1))

data N = Z | S N

data Spine xs where
  SpN :: Spine '[]
  Sp :: Spine xs -> Spine (x : xs)

deriving instance Show (Spine xs)

class KnownSpine xs where
  spine :: Spine xs

instance KnownSpine '[] where
  spine = SpN

instance KnownSpine xs => KnownSpine (x : xs) where
  spine = Sp spine

data Idx xs where
  IZ :: Idx (x : xs)
  IS :: Idx xs -> Idx (x : xs)

deriving instance Show (Idx xs)
deriving instance Eq (Idx xs)

idxSucc' :: Spine xs -> Idx xs -> Maybe (Idx xs)
idxSucc' sp IZ =
  case sp of
    Sp (Sp _) -> Just (IS IZ)
    _ -> Nothing
idxSucc' (Sp sp) (IS i) =
  IS <$> idxSucc' sp i

idxSucc :: KnownSpine xs => Idx xs -> Maybe (Idx xs)
idxSucc = idxSucc' spine

idxPred :: Idx xs -> Maybe (Idx xs)
idxPred IZ = Nothing
idxPred (IS IZ) = Just IZ
idxPred (IS i) = IS <$> idxPred i

data Idx_n xs n where
  IZ_n :: Idx_n (x : xs) Z
  IS_n :: Idx_n xs n -> Idx_n (x : xs) (S n)

data Some f where
  Some :: f a -> Some f

toSomeIdx :: Idx xs -> Some (Idx_n xs)
toSomeIdx IZ = Some IZ_n
toSomeIdx (IS i) =
  case toSomeIdx i of
    Some i' -> Some (IS_n i')

type (*) = Product
type Id = Identity

data Rec f xs where
  RNil :: Rec f '[]
  (:&) :: F.Unwrapped f x -> Rec f xs -> Rec f (x : xs)

infixr :&

zipWithIdx :: forall f xs. Rec f xs -> Rec (Const (Idx xs) * f) xs
zipWithIdx = go id
  where
    go :: forall xs'. (Idx xs' -> Idx xs) -> Rec f xs' -> Rec (Const (Idx xs) * f) xs'
    go _ RNil = RNil
    go f (x :& xs) = (f IZ, x) :& go (f . IS) xs

type HList = Rec Identity

type family AllConstrained c ts :: Constraint where
  AllConstrained c '[] = ()
  AllConstrained c (t ': ts) = (c t, AllConstrained c ts)

instance AllConstrained UndoEq xs => UndoEq (HList xs) where
  undoEq RNil RNil = True
  undoEq (x1 :& xs1) (x2 :& xs2) = undoEq x1 x2 && undoEq xs1 xs2

type family xs !! n where
  (x : _) !! Z = x
  (_ : xs) !! (S i) = xs !! i

class HasIdx_n n xs where
  idx_n :: Idx_n xs n

instance xs ~ (x':xs') => HasIdx_n Z xs where
  idx_n = IZ_n

instance (xs ~ (x':xs'), HasIdx_n n xs') => HasIdx_n (S n) xs where
  idx_n = IS_n idx_n

hlens :: Idx_n xs n -> Lens' (HList xs) (xs !! n)
hlens IZ_n = \f (x :& xs) -> fmap (:& xs) (f x)
hlens (IS_n i) = \f (x :& xs) -> fmap (x :&) (hlens i f xs)

type family Fields (label :: Type) :: [Type]

data SynRecord label =
  SynRecord
    { _synRec :: HList (Fields label),
      _synRecSel :: Idx (Fields label),
      _synRecSelSelf :: Bool
    }

makeLenses ''SynRecord

synField ::
  forall n label.
  HasIdx_n n (Fields label) =>
  Lens' (SynRecord label) (Fields label !! n)
synField = synRec . hlens (idx_n @n)

instance AllConstrained UndoEq (Fields label) => UndoEq (SynRecord label) where
  undoEq r1 r2 = undoEq (r1 ^. synRec) (r2 ^. synRec)

instance SynSelfSelected (SynRecord label)

instance xs ~ Fields label => SynSelection (SynRecord label) (Idx xs) where
  synSelection = synRecSel
  synSelectionSelf = synRecSelSelf

class SyntaxRecReact label where
  recChar :: Char
  recDefaultValue :: SynRecord label

deduceC ::
  forall c xs n r.
  AllConstrained c xs =>
  Idx_n xs n ->
  (c (xs !! n) => r) ->
  r
deduceC IZ_n r = r
deduceC (IS_n i) r = deduceC @c i r

recHandleSelRedirect ::
  forall rp la label.
  AllConstrained (SyntaxReact rp la) (Fields label) =>
  React rp la (SynRecord label)
recHandleSelRedirect = do
  False <- use synSelectionSelf
  Some sel <- uses synRecSel toSomeIdx
  deduceC @(SyntaxReact rp la) sel $
    reactRedirect (synRec . hlens sel)

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

handleArrowLeft :: SynSelection syn (Idx xs) => React rp la syn
handleArrowLeft = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowLeft 'h'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (idxPred selection)
  synSelection .= selection'

handleArrowRight :: KnownSpine xs => SynSelection syn (Idx xs) => React rp la syn
handleArrowRight = do
  guardInputEvent $ keyCodeLetter KeyCode.ArrowRight 'l'
  False <- use synSelectionSelf
  selection <- use synSelection
  selection' <- maybeA (idxSucc selection)
  synSelection .= selection'

handleArrows :: (KnownSpine xs, SynSelection syn (Idx xs)) => React rp la syn
handleArrows = asum @[]
  [handleArrowUp, handleArrowDown, handleArrowLeft, handleArrowRight]

instance
    ( AllConstrained (SyntaxReact rp Path) (Fields label),
      KnownSpine (Fields label),
      SyntaxRecReact label ) =>
    SyntaxReact rp Path (SynRecord label)
  where
    react = recHandleSelRedirect <|> handleArrows
    subreact = simpleSubreact (recChar @label) (recDefaultValue @label)

type CPS a = (a -> a) -> a

class SyntaxRecLayout label where
  recLayout ::
    s -/ Draw Path =>
    Rec (Const (CPS (Collage s))) (Fields label) ->
    Collage s

instance
    ( SyntaxRecLayout label,
      Typeable (Fields label),
      AllConstrained (SyntaxLayout Path LayoutCtx) (Fields label),
      AllConstrained SynSelfSelected (Fields label) ) =>
    SyntaxLayout Path LayoutCtx (SynRecord label)
  where
    layout syn =
      recLayout @label <$>
      fieldLayouts (zipWithIdx (syn ^. synRec))
      where
        fieldLayouts ::
          forall s xs.
          AllConstrained (SyntaxLayout Path LayoutCtx) xs =>
          AllConstrained SynSelfSelected xs =>
          s -/ Draw Path =>
          Rec (Const (Idx (Fields label)) * Id) xs ->
          Reader LayoutCtx (Rec (Const (CPS (Collage s))) xs)
        fieldLayouts RNil = pure RNil
        fieldLayouts ((sel', x) :& xs) = do
          let
            appendSelection =
              (lctxSelected &&~ (view synSelection syn == sel')) .
              (lctxSelected &&~ (synSelfSelected syn == False)) .
              (lctxPath %~ (`snoc` PathSegment typeRep sel'))
            enforceSelfSelection =
              lctxSelected &&~ synSelfSelected x
            xLayout :: Reader LayoutCtx (CPS (Collage s))
            xLayout = local appendSelection $ do
              a <- layout x
              local enforceSelfSelection $
                reader $ \lctx hook -> layoutSel lctx (hook a)
          (:&) <$> xLayout <*> fieldLayouts xs
