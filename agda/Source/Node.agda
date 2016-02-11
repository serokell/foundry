module Source.Node where

open import Relation.Nullary
open import Relation.Binary.Core
open import Data.Product
open import Data.Empty
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

  Field : {lk : LabelKind} → Label lk → Set → Set

  data Node (lk : LabelKind) (l : Label lk) (h : Set) : Set where
    node : Field l h → Node lk l h
    hole : h → Node lk l h

  Node' : ∃ Label → Set → Set
  Node' (lk , l) = Node lk l

  slave⟦_∣_⟧ : ∀{rk l} → (r : Relation rk l) → Set → Set
  slave⟦ r ∣ h ⟧ = Node' (slave r) h

  Field {lk = Π-lk} l h = (r : Relation Π-rk l) → slave⟦ r ∣ h ⟧
  Field {lk = Σ-lk} l h = Σ (Relation Σ-rk l) (λ r → slave⟦ r ∣ h ⟧)
  Field {lk = Ω-lk} l h = Ω-Field l

  ⟦_∣_⟧ : ∀{lk} → Label lk → Set → Set
  ⟦_∣_⟧ {lk} l h = Node lk l h

  pattern _node>_ r n = node (r , n)

  override : ∀{l h} → (r : Relation Π-rk l) → Endo slave⟦ r ∣ h ⟧ → Endo ⟦ l ∣ h ⟧
  override         _ _ (hole h)   = hole h
  override {l} {h} r f (node get) = node (f' ˢ get)
    where
      f' : (r₁ : Relation Π-rk l) → Endo slave⟦ r₁ ∣ h ⟧
      f' r₁ with r ≟rel r₁
      f' .r | yes refl = f
      f' r₁ | no _ = id

  module TraverseHole
    (traverse-Relation
       : ∀{l h}
       → Traverse-Relation
           (Relation Π-rk l)
           (λ r → slave⟦ r ∣ h ⟧))
    where

    traverseHole : ∀{lk l h h'} → Traversal (Node lk l h) (Node lk l h') h h'
    traverseHole {lk = Π-lk} f (node get)
      = node <$> traverse-Relation (λ r → traverseHole f (get r))
    traverseHole {lk = Σ-lk} f (r node> n) = _node>_ r <$> traverseHole f n
    traverseHole {lk = Ω-lk} _ (node a) = pure (node a)
    traverseHole f (hole x) = hole <$> (f x)

    mapHole : ∀{lk l h h'} → (h → h') → Node lk l h → Node lk l h'
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

  locate : ∀{p q} → Path p q → Traversal' (Node' p ⊥) (Node' q ⊥)
  locate <> f n = f n
  locate (_ ∶ _ > _) f (hole h) = pure (hole h)
  locate (Π-rk ∶ r > path) f (node get)
    = pure (λ a → override r (const a) (node get)) <*> locate path f (get r)
  locate (Σ-rk ∶ r > path) f (r₁ node> n) with r ≟rel r₁
  locate (Σ-rk ∶ r > path) f (.r node> n)
    | yes refl = pure (λ n₁ → r node> n₁) <*> locate path f n
  locate (Σ-rk ∶ r > path) f (r₁ node> n) | no _ = pure (r₁ node> n)
