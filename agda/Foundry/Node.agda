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
open import Foundry.Lens

Endo : ∀{n} → Set n → Set n
Endo s = s → s

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

open Syn Label Relation slave Ω-Field public

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

override : ∀{l} → (r : Relation Π-rk l) → Endo slave⟦ r ⟧ → Endo ⟦ l ⟧
override {l} r f (node get) = node (f' ˢ get)
  where
    f' : (r₁ : Relation Π-rk l) → Endo slave⟦ r₁ ⟧
    f' r₁ with r ≟rel r₁
    f' .r | yes refl = f
    f' r₁ | no _ = id

mkLam : ⟦ ⌝Arg ⟧ ⟦ ⌝Expr ⟧ ⟦ ⌝Expr ⟧ ⟦ ⌝Lam ⟧ < < <
mkLam arg expr1 expr2 = node λ
  { Lam-Arg → arg
  ; Lam-Expr₁ → expr1
  ; Lam-Expr₂ → expr2
  }

mkPi : ⟦ ⌝Arg ⟧ ⟦ ⌝Expr ⟧ ⟦ ⌝Expr ⟧ ⟦ ⌝Pi ⟧ < < <
mkPi arg expr1 expr2 = node λ
  { Pi-Arg → arg
  ; Pi-Expr₁ → expr1
  ; Pi-Expr₂ → expr2
  }

mkApp : ⟦ ⌝Expr ⟧ ⟦ ⌝Expr ⟧ ⟦ ⌝App ⟧ < <
mkApp expr1 expr2 = node λ
  { App-Expr₁ → expr1
  ; App-Expr₂ → expr2
  }

onExpr
  : ∀{n} {r : Set n}
  → ⟦ ⌝Const ⟧ r <
  → ⟦ ⌝Var   ⟧ r <
  → ⟦ ⌝Lam   ⟧ r <
  → ⟦ ⌝Pi    ⟧ r <
  → ⟦ ⌝App   ⟧ r <
  → ⟦ ⌝Embed ⟧ r <
  → ⟦ ⌝Expr  ⟧ r <
onExpr onConst onVar onLam onPi onApp onEmbed (r node> n) with r
... | Expr>Const = onConst n
... | Expr>Var   = onVar   n
... | Expr>Lam   = onLam   n
... | Expr>Pi    = onPi    n
... | Expr>App   = onApp   n
... | Expr>Embed = onEmbed n

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

↟ : ∀{l'} → Node' l' → String
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

    ↟Node : ∀{l'} → Node' l' → String
    ↟Node {_ , ⌝Const} (node c) = ↟Const c
    ↟Node {_ , ⌝Var} (node v) = ↟Var v
    ↟Node {_ , ⌝Embed} (node ())
    ↟Node {_ , ⌝Arg} (node arg) = ↟Arg arg
    ↟Node {_ , ⌝Expr} (_ node> n) = ↟Node n
    ↟Node {_ , ⌝Lam} (node get)
       = "λ" ++ ↟Node (get Lam-Arg)
      ++ ":" ++ ↟Node (get Lam-Expr₁)
      ++ "→" ++ ↟Node (get Lam-Expr₂)
    ↟Node {_ , ⌝Pi} (node get)
       = "Π" ++ ↟Node (get Pi-Arg)
      ++ ":" ++ ↟Node (get Pi-Expr₁)
      ++ "→" ++ ↟Node (get Pi-Expr₂)
    ↟Node {_ , ⌝App} (node get)
       = "(" ++ ↟Node (get App-Expr₁)
      ++ "|" ++ ↟Node (get App-Expr₂)
      ++ ")"

locate : ∀{p q} → Path p q → Traversal' (Node' p) (Node' q)
locate <> f n = f n
locate (Π-rk ∶ r > path) f (node get)
  = pure (λ a → override r (const a) (node get)) <*> locate path f (get r)
locate (Σ-rk ∶ r > path) f (r₁ node> n) with r ≟rel r₁
locate (Σ-rk ∶ r > path) f (.r node> n)
  | yes refl = pure (λ n₁ → r node> n₁) <*> locate path f n
locate (Σ-rk ∶ r > path) f (r₁ node> n) | no _ = pure (r₁ node> n)
