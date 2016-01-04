module Source.Node where

open import Relation.Nullary
open import Relation.Binary.Core
open import Data.Product
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

module Syn
  (Label : LabelKind → Set)
  (Relation : (rk : RelationKind) → Label (rk-lk rk) → Set)
  (_≟rel_ : ∀{rk l} → Decidable {A = Relation rk l} _≡_)
  (slave : ∀{rk l} → Relation rk l → ∃ Label)
  (Ω-Field : Label Ω-lk → Set)
  where

  Field : {lk : LabelKind} → Label lk → Set

  data Node (lk : LabelKind) (l : Label lk) : Set where
    node : Field l → Node lk l

  Node' : ∃ Label → Set
  Node' (lk , l) = Node lk l

  slave⟦_⟧ : ∀{rk l} → (r : Relation rk l) → Set
  slave⟦_⟧ r = Node' (slave r)

  Field {lk = Π-lk} l = (r : Relation Π-rk l) → slave⟦ r ⟧
  Field {lk = Σ-lk} l = Σ (Relation Σ-rk l) slave⟦_⟧
  Field {lk = Ω-lk} = Ω-Field

  ⟦_⟧ : ∀{lk} → Label lk → Set
  ⟦_⟧ {lk} l = Node lk l

  ⟦_⟧_< : ∀{n lk} → Label lk → (t : Set n) → Set n
  ⟦_⟧_< l t = ⟦ l ⟧ → t

  pattern _node>_ r n = node (r , n)

  override : ∀{l} → (r : Relation Π-rk l) → Endo slave⟦ r ⟧ → Endo ⟦ l ⟧
  override {l} r f (node get) = node (f' ˢ get)
    where
        f' : (r₁ : Relation Π-rk l) → Endo slave⟦ r₁ ⟧
        f' r₁ with r ≟rel r₁
        f' .r | yes refl = f
        f' r₁ | no _ = id

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

  locate : ∀{p q} → Path p q → Traversal' (Node' p) (Node' q)
  locate <> f n = f n
  locate (Π-rk ∶ r > path) f (node get)
    = pure (λ a → override r (const a) (node get)) <*> locate path f (get r)
  locate (Σ-rk ∶ r > path) f (r₁ node> n) with r ≟rel r₁
  locate (Σ-rk ∶ r > path) f (.r node> n)
    | yes refl = pure (λ n₁ → r node> n₁) <*> locate path f n
  locate (Σ-rk ∶ r > path) f (r₁ node> n) | no _ = pure (r₁ node> n)
