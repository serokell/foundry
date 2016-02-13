module Source.Node where

open import Relation.Nullary
open import Relation.Binary.Core
open import Data.Product
open import Data.Empty
open import Data.Unit
open import Function

open import Source.Lens

data RelationKind : Set where
  Π-rk Σ-rk : RelationKind

data LabelKind : Set where
  rk-lk : RelationKind → LabelKind
  Ω-lk : LabelKind

pattern Π-lk = rk-lk Π-rk
pattern Σ-lk = rk-lk Σ-rk

Endo : ∀{n} → Set n → Set n
Endo s = s → s

Traverse-Relation : (R : Set) → (G : R → Set) → Set₁
Traverse-Relation R G
  = ∀ {F} {{appF : Applicative F}}
  → ((r : R) → F (G r))
  → F ((r : R) → G r)

module Syn
  (Label : LabelKind → Set)
  (Relation : (rk : RelationKind) → Label (rk-lk rk) → Set)
  (_≟rel_ : ∀{rk l} → Decidable {A = Relation rk l} _≡_)
  (slave : ∀{rk l} → Relation rk l → ∃ Label)
  (Ω-Field : Label Ω-lk → Set)
  where

  Field : {lk : LabelKind} → Label lk → Set → (∀{lk₁} → Label lk₁ → Set) → Set

  -- TODO: h : ∃ Label → Set
  data Node (lk : LabelKind) (l : Label lk) (h : Set) (ann : ∀{lk₁} → Label lk₁ → Set) : Set where
    node : Field l h ann → ann l → Node lk l h ann
    hole : h → Node lk l h ann

  Node' : ∃ Label → Set → (∀{lk₁} → Label lk₁ → Set) → Set
  Node' (lk , l) = Node lk l

  slave⟦_∣_∣_⟧ : ∀{rk l} → (r : Relation rk l) → Set → (∀{lk₁} → Label lk₁ → Set) → Set
  slave⟦ r ∣ h ∣ ann ⟧ = Node' (slave r) h ann

  Field {lk = Π-lk} l h ann = (r : Relation Π-rk l) → slave⟦ r ∣ h ∣ ann ⟧
  Field {lk = Σ-lk} l h ann = Σ (Relation Σ-rk l) (λ r → slave⟦ r ∣ h ∣ ann ⟧)
  Field {lk = Ω-lk} l h ann = Ω-Field l

  ⟦_∣_∣_⟧ : ∀{lk} → Label lk → Set → (∀{lk₁} → Label lk₁ → Set) → Set
  ⟦_∣_∣_⟧ {lk} = Node lk

  pattern _node>_∣_ r n a = node (r , n) a

  override
    : ∀{l h} {ann : ∀{lk₁} → Label lk₁ → Set}
    → (r : Relation Π-rk l)
    → Endo slave⟦ r ∣ h ∣ ann ⟧
    → Endo      ⟦ l ∣ h ∣ ann ⟧
  override               _ _ (hole h)     = hole h
  override {l} {h} {ann} r f (node get a) = node (f' ˢ get) a
    where
      f' : (r₁ : Relation Π-rk l) → Endo slave⟦ r₁ ∣ h ∣ ann ⟧
      f' r₁ with r ≟rel r₁
      f' .r | yes refl = f
      f' r₁ | no _ = id

  module TraverseSyn
    (traverse-Relation
       : ∀{l h} {ann : ∀{lk₁} → Label lk₁ → Set}
       → Traverse-Relation
           (Relation Π-rk l)
           (λ r → slave⟦ r ∣ h ∣ ann ⟧))
    where

    traverseHole
      : ∀{lk l h h'} {ann : ∀{lk₁} → Label lk₁ → Set}
      → Traversal (Node lk l h ann) (Node lk l h' ann) h h'
    traverseHole {lk = Π-lk} f (node get a)
      = (λ get₁ → node get₁ a) <$> traverse-Relation (λ r → traverseHole f (get r))
    traverseHole {lk = Σ-lk} f (r node> n ∣ a) = (λ n₁ → r node> n₁ ∣ a) <$> traverseHole f n
    traverseHole {lk = Ω-lk} _ (node x a) = pure (node x a)
    traverseHole f (hole x) = hole <$> f x

    mapHole
      : ∀{lk l h h'} {ann : ∀{lk₁} → Label lk₁ → Set}
      → (h → h')
      → Node lk l h  ann
      → Node lk l h' ann
    mapHole = over traverseHole

  data Path : (p q : ∃ Label) → Set where
    <> : {p : ∃ Label} → Path p p
    _∶_>_
      : (rk : RelationKind)
      → ∀{p q}
      → (r : Relation rk p)
      → Path (slave r) q
      → Path (, p) q

  pattern ⟪_⟫ r = _ ∶ r > <>

  _>->_ : ∀{p q q'} → Path p q → Path q q' → Path p q'
  <> >-> path₂ = path₂
  (r ∶ rk > path₁) >-> path₂ = r ∶ rk > (path₁ >-> path₂)

  infixr 9 _>->_

  locate
    : ∀{p q h} {ann : ∀{lk₁} → Label lk₁ → Set}
    → Path p q
    → Traversal' (Node' p h ann) (Node' q h ann)
  locate <> f n = f n
  locate (_ ∶ _ > _) f (hole h) = pure (hole h)
  locate (Π-rk ∶ r > path) f (node get a)
    = pure (λ x → override r (const x) (node get a)) <*> locate path f (get r)
  locate (Σ-rk ∶ r > path) f (r₁ node> n ∣ a) with r ≟rel r₁
  locate (Σ-rk ∶ r > path) f (.r node> n ∣ a)
    | yes refl = pure (λ n₁ → r node> n₁ ∣ a) <*> locate path f n
  locate (Σ-rk ∶ r > path) f (r₁ node> n ∣ a) | no _ = pure (r₁ node> n ∣ a)
