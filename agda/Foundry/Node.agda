module Foundry.Node where

open import Data.Product
open import Data.String
open import Data.Nat
open import Data.Empty
open import Data.Unit
open import Data.Maybe
open import Relation.Nullary
open import Relation.Nullary.Decidable
open import Relation.Binary.Core
open import Relation.Binary.PropositionalEquality
open import Function

open import Source.Node
open import Source.Lens

data Label : LabelKind → Set where
  ⌝Lam ⌝Pi ⌝App : Label Π-lk
  ⌝Expr : Label Σ-lk
  ⌝Const ⌝Var ⌝Embed ⌝Arg : Label Ω-lk

data Relation : (rk : RelationKind) → Label (rk-lk rk) → Set where
  -- Π-rk
  Lam-Arg Lam-Expr₁ Lam-Expr₂ : Relation Π-rk ⌝Lam
  Pi-Arg Pi-Expr₁ Pi-Expr₂ : Relation Π-rk ⌝Pi
  App-Expr₁ App-Expr₂ : Relation Π-rk ⌝App
  -- Σ-rk
  Expr>Const Expr>Var Expr>Lam Expr>Pi Expr>App Expr>Embed : Relation Σ-rk ⌝Expr

slave : ∀{rk l} → Relation rk l → ∃ Label
slave {Π-rk} {⌝Lam} = λ
  { Lam-Arg → , ⌝Arg
  ; Lam-Expr₁ → , ⌝Expr
  ; Lam-Expr₂ → , ⌝Expr
  }
slave {Π-rk} {⌝Pi} = λ
  { Pi-Arg → , ⌝Arg
  ; Pi-Expr₁ → , ⌝Expr
  ; Pi-Expr₂ → , ⌝Expr
  }
slave {Π-rk} {⌝App} = λ
  { App-Expr₁ → , ⌝Expr
  ; App-Expr₂ → , ⌝Expr
  }
slave {Σ-rk} {⌝Expr} = λ
  { Expr>Const → , ⌝Const
  ; Expr>Var → , ⌝Var
  ; Expr>Lam → , ⌝Lam
  ; Expr>Pi → , ⌝Pi
  ; Expr>App → , ⌝App
  ; Expr>Embed → , ⌝Embed
  }

data Const : Set where
  Star Box : Const

record Arg : Set where
  constructor Argument
  field
    arg-name : String

record Var : Set where
  constructor Variable
  field
    var-index : ℕ
    var-arg : Arg

Ω-Field : Label Ω-lk → Set
Ω-Field = λ
  { ⌝Const → Const
  ; ⌝Var → Var
  ; ⌝Embed → ⊥
  ; ⌝Arg → Arg
  }

-- TODO: derive?
_≟rel_ : ∀{rk l} → Decidable {A = Relation rk l} _≡_
_≟rel_ Lam-Arg Lam-Arg = yes refl
_≟rel_ Lam-Arg Lam-Expr₁ = no λ()
_≟rel_ Lam-Arg Lam-Expr₂ = no λ()
_≟rel_ Lam-Expr₁ Lam-Arg = no λ()
_≟rel_ Lam-Expr₁ Lam-Expr₁ = yes refl
_≟rel_ Lam-Expr₁ Lam-Expr₂ = no λ()
_≟rel_ Lam-Expr₂ Lam-Arg = no λ()
_≟rel_ Lam-Expr₂ Lam-Expr₁ = no λ()
_≟rel_ Lam-Expr₂ Lam-Expr₂ = yes refl
_≟rel_ Pi-Arg Pi-Arg = yes refl
_≟rel_ Pi-Arg Pi-Expr₁ = no λ()
_≟rel_ Pi-Arg Pi-Expr₂ = no λ()
_≟rel_ Pi-Expr₁ Pi-Arg = no λ()
_≟rel_ Pi-Expr₁ Pi-Expr₁ = yes refl
_≟rel_ Pi-Expr₁ Pi-Expr₂ = no λ()
_≟rel_ Pi-Expr₂ Pi-Arg = no λ()
_≟rel_ Pi-Expr₂ Pi-Expr₁ = no λ()
_≟rel_ Pi-Expr₂ Pi-Expr₂ = yes refl
_≟rel_ App-Expr₁ App-Expr₁ = yes refl
_≟rel_ App-Expr₁ App-Expr₂ = no λ()
_≟rel_ App-Expr₂ App-Expr₁ = no λ()
_≟rel_ App-Expr₂ App-Expr₂ = yes refl
_≟rel_ Expr>Const Expr>Const = yes refl
_≟rel_ Expr>Const Expr>Var = no λ()
_≟rel_ Expr>Const Expr>Lam = no λ()
_≟rel_ Expr>Const Expr>Pi = no λ()
_≟rel_ Expr>Const Expr>App = no λ()
_≟rel_ Expr>Const Expr>Embed = no λ()
_≟rel_ Expr>Var Expr>Const = no λ()
_≟rel_ Expr>Var Expr>Var = yes refl
_≟rel_ Expr>Var Expr>Lam = no λ()
_≟rel_ Expr>Var Expr>Pi = no λ()
_≟rel_ Expr>Var Expr>App = no λ()
_≟rel_ Expr>Var Expr>Embed = no λ()
_≟rel_ Expr>Lam Expr>Const = no λ()
_≟rel_ Expr>Lam Expr>Var = no λ()
_≟rel_ Expr>Lam Expr>Lam = yes refl
_≟rel_ Expr>Lam Expr>Pi = no λ()
_≟rel_ Expr>Lam Expr>App = no λ()
_≟rel_ Expr>Lam Expr>Embed = no λ()
_≟rel_ Expr>Pi Expr>Const = no λ()
_≟rel_ Expr>Pi Expr>Var = no λ()
_≟rel_ Expr>Pi Expr>Lam = no λ()
_≟rel_ Expr>Pi Expr>Pi = yes refl
_≟rel_ Expr>Pi Expr>App = no λ()
_≟rel_ Expr>Pi Expr>Embed = no λ()
_≟rel_ Expr>App Expr>Const = no λ()
_≟rel_ Expr>App Expr>Var = no λ()
_≟rel_ Expr>App Expr>Lam = no λ()
_≟rel_ Expr>App Expr>Pi = no λ()
_≟rel_ Expr>App Expr>App = yes refl
_≟rel_ Expr>App Expr>Embed = no λ()
_≟rel_ Expr>Embed Expr>Const = no λ()
_≟rel_ Expr>Embed Expr>Var = no λ()
_≟rel_ Expr>Embed Expr>Lam = no λ()
_≟rel_ Expr>Embed Expr>Pi = no λ()
_≟rel_ Expr>Embed Expr>App = no λ()
_≟rel_ Expr>Embed Expr>Embed = yes refl

open Syn Label Relation _≟rel_ slave Ω-Field public

mkLam
  : ∀{h} {ann : ∀{lk₁} → Label lk₁ → Set}
  → ⟦ ⌝Arg  ∣ h ∣ ann ⟧
  → ⟦ ⌝Expr ∣ h ∣ ann ⟧
  → ⟦ ⌝Expr ∣ h ∣ ann ⟧
  → Field ⌝Lam h ann
mkLam arg expr1 expr2 = λ
  { Lam-Arg → arg
  ; Lam-Expr₁ → expr1
  ; Lam-Expr₂ → expr2
  }

mkPi
  : ∀{h} {ann : ∀{lk₁} → Label lk₁ → Set}
  → ⟦ ⌝Arg  ∣ h ∣ ann ⟧
  → ⟦ ⌝Expr ∣ h ∣ ann ⟧
  → ⟦ ⌝Expr ∣ h ∣ ann ⟧
  → Field ⌝Pi h ann
mkPi arg expr1 expr2 = λ
  { Pi-Arg → arg
  ; Pi-Expr₁ → expr1
  ; Pi-Expr₂ → expr2
  }

mkApp
  : ∀{h} {ann : ∀{lk₁} → Label lk₁ → Set}
  → ⟦ ⌝Expr ∣ h ∣ ann ⟧
  → ⟦ ⌝Expr ∣ h ∣ ann ⟧
  → Field ⌝App h ann
mkApp expr1 expr2 = λ
  { App-Expr₁ → expr1
  ; App-Expr₂ → expr2
  }

traverse-Relation
  : ∀{l h} {ann : ∀{lk₁} → Label lk₁ → Set}
  → Traverse-Relation (Relation Π-rk l) (λ r → slave⟦ r ∣ h ∣ ann ⟧)
traverse-Relation {⌝Lam} f = mkLam <$> f Lam-Arg   <*> f Lam-Expr₁ <*> f Lam-Expr₂
traverse-Relation {⌝Pi}  f = mkPi  <$> f  Pi-Arg   <*> f  Pi-Expr₁ <*> f  Pi-Expr₂
traverse-Relation {⌝App} f = mkApp <$> f App-Expr₁ <*> f App-Expr₂

open TraverseSyn traverse-Relation public

onExpr
  : ∀{n} {r : Set n}
  → (⟦ ⌝Const ∣ ⊥ ∣ const ⊤ ⟧ → r)
  → (⟦ ⌝Var   ∣ ⊥ ∣ const ⊤ ⟧ → r)
  → (⟦ ⌝Lam   ∣ ⊥ ∣ const ⊤ ⟧ → r)
  → (⟦ ⌝Pi    ∣ ⊥ ∣ const ⊤ ⟧ → r)
  → (⟦ ⌝App   ∣ ⊥ ∣ const ⊤ ⟧ → r)
  → (⟦ ⌝Embed ∣ ⊥ ∣ const ⊤ ⟧ → r)
  → (⟦ ⌝Expr  ∣ ⊥ ∣ const ⊤ ⟧ → r)
onExpr _ _ _ _ _ _ (hole h) = ⊥-elim h
onExpr onConst onVar onLam onPi onApp onEmbed (r node> n ∣ tt) with r
... | Expr>Const = onConst n
... | Expr>Var   = onVar   n
... | Expr>Lam   = onLam   n
... | Expr>Pi    = onPi    n
... | Expr>App   = onApp   n
... | Expr>Embed = onEmbed n

↟ : ∀{l'} → Node' l' ⊥ (const ⊤) → String
↟ = ↟Node

  where

    ↟ℕ-index : ℕ → String
    ↟ℕ-index zero = "₀"
    ↟ℕ-index (suc n) = "₊" ++ ↟ℕ-index n

    ↟Arg : Arg → String
    ↟Arg (Argument s) = s

    ↟Const : Const → String
    ↟Const = λ { Star → "⋆"; Box → "□" }

    ↟Var : Var → String
    ↟Var (Variable n arg) = ↟Arg arg ++ ↟ℕ-index n

    ↟Node : ∀{l'} → Node' l' ⊥ (const ⊤) → String
    ↟Node (hole h) = ⊥-elim h
    ↟Node {_ , ⌝Const} (node c tt) = ↟Const c
    ↟Node {_ , ⌝Var} (node v tt) = ↟Var v
    ↟Node {_ , ⌝Embed} (node () tt)
    ↟Node {_ , ⌝Arg} (node arg tt) = ↟Arg arg
    ↟Node {_ , ⌝Expr} (_ node> n ∣ tt) = ↟Node n
    ↟Node {_ , ⌝Lam} (node get tt)
       = "λ" ++ ↟Node (get Lam-Arg)
      ++ ":" ++ ↟Node (get Lam-Expr₁)
      ++ "→" ++ ↟Node (get Lam-Expr₂)
    ↟Node {_ , ⌝Pi} (node get tt)
       = "Π" ++ ↟Node (get Pi-Arg)
      ++ ":" ++ ↟Node (get Pi-Expr₁)
      ++ "→" ++ ↟Node (get Pi-Expr₂)
    ↟Node {_ , ⌝App} (node get tt)
       = "(" ++ ↟Node (get App-Expr₁)
      ++ "|" ++ ↟Node (get App-Expr₂)
      ++ ")"
