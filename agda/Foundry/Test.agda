module Foundry.Test where

open import Data.Maybe
open import Relation.Binary.PropositionalEquality
open import Function

open import Source.Lens
open import Foundry.Node

path₀ : _ -- Path (, ⌝Expr) (, ⌝Const)
path₀ = ⟪ Expr>Lam ⟫ >-> ⟪ Lam-Expr₁ ⟫ >-> ⟪ Expr>Const ⟫ >-> <>

expr₀ : ⟦ ⌝Expr ⟧
expr₀ =
  Expr>Lam node> mkLam
    (node (Argument "star"))
    (Expr>Const node> node Star)
    (Expr>App node> mkApp
      (Expr>Var node> node (Variable 0 (Argument "star")))
      (Expr>Const node> node Star)
    )

expr₁ : _ -- ⟦ ⌝Expr ⟧
expr₁ = over (locate path₀) (const (node Box)) expr₀

locate-set-get-test₁ : view? (locate path₀) expr₁ ≡ just (node Box)
locate-set-get-test₁ = refl
