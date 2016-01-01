module Source.Node where

open import Data.Product

data RelationKind : Set where
  Π-rk Σ-rk : RelationKind

data LabelKind : Set where
  rk-lk : RelationKind → LabelKind
  Ω-lk : LabelKind

pattern Π-lk = rk-lk Π-rk
pattern Σ-lk = rk-lk Σ-rk

module Syn
  (Label : LabelKind → Set)
  (Relation : (rk : RelationKind) → Label (rk-lk rk) → Set)
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
