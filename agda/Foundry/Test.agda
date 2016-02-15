module Foundry.Test where

open import Data.Maybe
open import Relation.Binary.PropositionalEquality
open import Function
open import Data.Empty
open import Data.Unit

open import Source.Lens
open import Foundry.Node

path₀ : _ -- Path (, ⌝Expr) (, ⌝Const)
path₀ = ⟪ Expr>Lam ⟫ >-> ⟪ Lam-Expr₁ ⟫ >-> ⟪ Expr>Const ⟫ >-> <>

expr₀ : ⟦ ⌝Expr ∣ const ⊥ ∣ const ⊤ ⟧
expr₀ =
  Expr>Lam node> node (mkLam
    (node (Argument "star") tt)
    (Expr>Const node> node Star tt ∣ tt)
    (Expr>App node> node (mkApp
      (Expr>Var node> node (Variable 0 (Argument "star")) tt ∣ tt)
      (Expr>Const node> node Star tt ∣ tt)) tt ∣ tt
    )) tt ∣ tt

expr₁ : _ -- ⟦ ⌝Expr ∣ const ⊥ ∣ const ⊤ ⟧
expr₁ = over (locate path₀) (const (node Box tt)) expr₀

locate-set-get-test₁ : view? (locate path₀) expr₁ ≡ just (node Box tt)
locate-set-get-test₁ = refl
