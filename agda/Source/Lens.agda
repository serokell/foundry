module Source.Lens where

open import Data.Maybe
open import Function
open import Level

record Applicative {ℓ} (F : Set ℓ → Set ℓ) : Set (suc ℓ) where
  infixl 4 _<*>_
  field
    _<*>_ : ∀ {A B} → F (A → B) → F A → F B
    pure : ∀{A} → A → F A

open Applicative{{...}} public

private

  data Preview (e : Set) (a : Set) : Set where
    veil : Preview e a
    preview : e → Preview e a

  preview-to-maybe : ∀{a e} → Preview e a → Maybe e
  preview-to-maybe veil = nothing
  preview-to-maybe (preview x) = just x

  data Identity (a : Set) : Set where
    identity : a → Identity a

  identity-to-content : ∀{a} → Identity a → a
  identity-to-content (identity a) = a

  Identity-Applicative : Applicative Identity
  Identity-Applicative = record
    { _<*>_ = _<*>-identity_
    ; pure = pure-identity
    } where

      pure-identity : ∀{a} → a → Identity a
      pure-identity = identity

      _<*>-identity_ : ∀{a b} → Identity (a → b) → Identity a → Identity b
      identity f <*>-identity identity x = identity (f x)

  Preview-Applicative : ∀{e} → Applicative (Preview e)
  Preview-Applicative = record
    { _<*>_ = _<*>-preview_
    ; pure = pure-preview
    } where

      pure-preview : ∀{a e} → a → Preview e a
      pure-preview _ = veil

      _<*>-preview_ : ∀{a b e} → Preview e (a → b) → Preview e a → Preview e b
      veil <*>-preview veil = veil
      veil <*>-preview preview x = preview x
      preview x <*>-preview _ = preview x

Traversal' : Set → Set → Set _
Traversal' s a = ({F : Set → Set} → {{app : Applicative F}} → (a → F a) → s → F s)

view? : ∀{s a} → Traversal' s a → s → Maybe a
view? l = preview-to-maybe ∘ l {{Preview-Applicative}} preview

over : ∀{s a} → Traversal' s a → (a → a) → (s → s)
over l f = identity-to-content ∘ l {{Identity-Applicative}} (identity ∘ f)
