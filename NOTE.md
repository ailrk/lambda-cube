### Lambda calculus

Untyped lambda calculus. The syntax is defined as:
```
e = x                       variable
  | e1 e2                   application
  | λx. e                   abstraction
  | let x = e1 in e2        let extension
```

##### Reduction rules

**α conversion**
Systematically change the parameter in lambda body.
``

**β reduction**
lambda application
`(λV.M)N ≡ M[V:=N]`

**η reduction**
local completeness.
`(λx.f x) ≡ f`


### Lambda Cubes
Learn some type systems by doing.

    λω ---------λC
   /|           /|
  / |          / |
 λ2 --------λP2  |
 |  |        |   |
 |  λω_ -----|- λPω_
 |  /        |  /
 | /         | /
 λ→ ---------λP

Four levels:
- term
- type
- kind
- □

Three directions corresponds to three types of extensions.
- ↑: polymorphism (terms bind types)
- →: dependent type (types bind terms)
- /: type operators (types bind types)
##### Simply typed lambda calculus (λ→)
- monotype
- term depends on term
```
  Γ, x:σ ⊢ t:τ
----------------
  Γ ⊢ λx.t:σ→τ
```

Lambda expressions have type now.

##### System F (λ2)
- aka "second order typed lambda calculus"
- 2 is for second order typed lambda calculus
- added universal quantification for introducing type variable
- quantification allows parametric polymorphic types
- corresponds to only part of second order intuionistic logic (with only ∀)
- term depends on type
```
  Γ ⊢ t:σ
-----------
Γ ⊢ ∀α.t:Πα.σ
```

##### System Fω_ (λω_)
- have type level operator
- types binds terms
- with type constructors
- introduce kind system (you might have τ :: * → * that takes a type to complete)

This one is super weird. If you don't have polymorphism but have type operator to map types from one to another, what's the parameter of the type?

##### Lambda P (λP)
- aka ΛΠ
- dependent type (types depends on terms)
-
```
  Γ,x:A ⊢ B:*
---------------
 Γ ⊢ (Πx:A.B):*
```

##### System Fω (λω)
- aka "higher order polymorphic lambda calculus"
- allows universal quantification, abstraction, and application at higher kind.
- the type level itself now is a simply typed lambda calculus

##### System FC (λC)
- types look like term now.


##### Hindely Minler
Add some restrictions to make system F decidable. You don't have higher rank type `(∀α. α → α) → (∀α. α → α)`, but now you have decidable global inference.

**Types**
There are monotypes and polytypes in
```
mono τ = α                  variable
       | C τ ... τ          application
       | τ → τ              abstraction

poly σ = τ
       | ∀α.σ               quantifier
```

**Context**
- Context is just a list of variable, type pairs.
- typing is just to assert a variable, type pair from the context.
```
Context   Γ = ε
            | Γ, x:σ
Typing    Γ ⊢ e:σ
```

### Pure type system (PTS)

A generalization of lambda cube, allows any number of sorts and dependencies between them.

**Big idea:**
```
  Γ, x:A ⊢ b:B
----------------------
Γ ⊢ (λx:A.b):(Πx:A.B)
```

###### pts triple and type rules

pts is defined as a triple `(S, A, R)`, where `S` is the set of sort, `A⊆S²` is the set of axioms, and `R⊆S³` is the set of rules.


```
(s₁, s₂) ∈ A
-------------   (axiom)
  ⊢ s₁:s₂

Γ ⊢ A:s x∉dom(Γ)
-------------------   9(start)
  Γ, x:A ⊢ x:A
```
