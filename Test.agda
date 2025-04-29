module Test where

open import Agda.Builtin.Nat
  using (Nat; _+_; _*_; suc; zero)
open import Agda.Builtin.Equality
  using (_≡_; refl)
-- open import Data.List

-- empty type.
data ⊥ : Set where

~ : Set → Set
~ A = A → ⊥

data _⊔_ (A B : Set) : Set where
  left : A  → A ⊔ B
  right : B → A ⊔ B
infixr 1 _⊔_

data _<_ : Nat → Nat → Set where
  zero : ∀ {b}
    → zero < suc b
  suc : ∀ {a b} → a < b
    → suc a < suc b
infix 4 _<_
-- ### Divisibility

-- a definition of a | b.
record Div (a b : Nat) : Set where
  constructor
    divi
  field
    nat
      : Nat
    times
      : nat * a ≡ b
-- ### Proper divisors

-- represents a divisor of n between 1 & n.
record ProperDiv (n : Nat) : Set where
  constructor
    prop-div
  field
    nat
      : Nat
    div
      : Div nat n
    greater
      : 1 < nat
    less
      : nat < n

-- ### Primality

-- represents a prime number.
record Prime (p : Nat) : Set where
  constructor
    prim
  field
    less
      : 1 < p
    proper-div
      : ~ (ProperDiv p)

-- ### Prime divisors

-- represents a prime divisor of n.
record PrimeDiv (n : Nat) : Set where
  constructor
    pdiv
  field
    nat
      : Nat
    div
      : Div nat n
    prime
      : Prime nat


data Vec (A : Set) : Nat → Set where
  [] : Vec A zero
  _∷_ : ∀ {n} → A → Vec A n → Vec A (suc n)

postulate

  prime-div : ∀ n → (1 < n) → PrimeDiv n


data All {A : Set} (P : A → Set) : ∀ {n} → Vec A n → Set where
  nil : All P []
  cons : ∀ {n x xs} → P x → All P {n} xs → All P (x ∷ xs)


all-map : {A : Set} → {P Q : A → Set} → ∀ {n} → {xs : Vec A n} 
  → (∀ x → P x → Q x) → All P xs → All Q xs
all-map f nil = nil
all-map f (cons {x = x} y b) = cons (f x y) (all-map f b)

product : {n : Nat} → Vec Nat n → Nat
product [] = 1 -- initially had as 0 but got an error; fixed with ChatGPT which explained why the base case should be 1
product (x ∷ ns) = x * (product ns)

-- make helper function to fill in hole below with this as the type: (x : Nat) → Prime x → 0 < x
helper : (x : Nat) → Prime x → 0 < x
helper zero ()
helper (suc x) (prim less proper-div) = zero

times-nonzero : ∀ m n → 0 < m → 0 < n → 0 < m * n
times-nonzero (suc a) (suc b) zero zero = zero

product-nonzero : ∀ {n} → {xs : Vec Nat n} → All (λ n → 0 < n) xs → 0 < product xs
product-nonzero nil = zero
product-nonzero {b} {x ∷ xs} (cons l ls) = times-nonzero x (product xs) l (product-nonzero ls) 

new-prime : ∀ {n} → (xs : Vec Nat n) → All Prime xs → Nat
new-prime ns ps = PrimeDiv.nat (prime-div (suc (product ns)) (suc (product-nonzero (all-map (λ x x₁ → helper x x₁) ps))))

primes : (n : Nat) → Vec Nat n
primes-valid : (n : Nat) → All Prime (primes n)

primes 0 = []
primes (suc n) = new-prime (primes n) (primes-valid n) ∷ primes n

primes-valid-helper : ∀ {n} -> (xs : Vec Nat n) -> (pv : All Prime xs) -> ~ (ProperDiv (new-prime (xs) pv))
primes-valid-helper {.0} [] pval (prop-div nat div greater less) = {!   !}
primes-valid-helper (x₁ ∷ xs) pv (prop-div nat div greater less) = {!   !}



primes-valid 0 = nil
primes-valid (suc n) = cons (prim (Prime.less (prim (Prime.less (prim {!   !} ((primes-valid-helper (primes n) (primes-valid n))))) (primes-valid-helper (primes n) (primes-valid n)))) ((primes-valid-helper (primes n) (primes-valid n)))) (primes-valid n) 


data Element {A : Set} (x : A) : ∀ {n} → Vec A n → Set where
  zero : ∀ {n xs} → Element x {suc n} (x ∷ xs)
  suc : ∀ {n y xs} → Element x {n} xs → Element x (y ∷ xs)

-- make helper function called distinct
data Distinct {A : Set} : ∀ {n} → Vec A n → Set where
  nil : Distinct []
  cons : ∀ {n x xs} → ~ (Element x {n} xs) → Distinct xs → Distinct (x ∷ xs)

-- got idea and definition for new-primes-not-in from ChatGPT 

new-primes-not-in : ∀ {n} -> (xs : Vec Nat n) -> (pval : All Prime xs) ->  ~ (Element (new-prime xs pval) xs)
new-primes-not-in [] pval ()
new-primes-not-in (x₁ ∷ xs) pval x = {!   !} 

primes-distinct : (n : Nat) → Distinct (primes n)
primes-distinct 0 = nil
primes-distinct (suc n) = cons (new-primes-not-in (primes n) (primes-valid n)) (primes-distinct n)
 