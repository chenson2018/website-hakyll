---
title: The Interplay Between Metaprogramming and Computation in Lean
tags: Proof Assistants, Lean, Metaprogramming
---

This week and next I am at the [Oregon Programming Languages Summer School
(OPLSS)](https://www.cs.uoregon.edu/research/summerschool/summer25/index.php).
With well over a hundred participants across academia and industry I have had
ample opportunities to explain my research, including my choice to primarily use
Lean for formalizations. One of the many reasons is its metaprogramming system,
which to my knowledge is unique among proof assistants. I don't claim to be a
fully fledged expert in Lean's internals, but an example in one of [Nada
Amin's](https://namin.seas.harvard.edu/)
[lectures](https://github.com/namin/metaprogramming) prompted me to write this
short blog post covering some basics.

In some sense, **every piece of Lean code is a metaprogram**, as Lean 4 is
self-hosted. Every Lean program takes an internally defined inductive type of
Lean syntax and transforms it into another inductive type representing a Lean
expression[^syntaxexpr]. This process is called **elaboration**. This is not
limited to just constructs of the language, but is extensible by the end-user
who is provided the ability to create fully custom syntaxes and elaborations,
written as plain Lean code within a hierarchy of monads[^meta].

[^syntaxexpr]: See
    [Lean.TSyntax](https://leanprover-community.github.io/mathlib4_docs/Init/Prelude.html#Lean.TSyntax),
    [Lean.Syntax](https://leanprover-community.github.io/mathlib4_docs/Init/Prelude.html#Lean.Syntax),
    and
    [Lean.Expr](https://leanprover-community.github.io/mathlib4_docs/Lean/Expr.html#Lean.Expr)

[^meta]: The [Metaprogramming in Lean
    4](https://leanprover-community.github.io/lean4-metaprogramming-book/main/01_intro.html)
    book is a great introductory resource, but unfortunately much of this part
    of the language is very sparsely documented. There is however a [dedicated
    channel for
    metaprogramming](https://leanprover.zulipchat.com/#narrow/channel/239415-metaprogramming-.2F-tactics/)
    on the Lean Zulip where very experienced Lean users are often available and
    willing to help!

We encountered the following in yesterday's lecture[^live],
```lean
-- Object level: inductively defined Even predicate
inductive Even : Nat → Prop where
  | zero : Even 0
  | succ_succ : ∀ n, Even n → Even (n + 2)

-- Meta level: computational even checker
def isEven : Nat → Bool
  | 0 => true
  | 1 => false
  | n + 2 => isEven n

theorem isEven_iff {n : Nat} : isEven n = true ↔ Even n := by
  constructor
  · fun_induction isEven n <;> simp +contextual [Even.zero, Even.succ_succ, *]
  · intro h; induction h <;> simp [isEven, *]

instance (n : Nat) : Decidable (Even n) :=
  decidable_of_decidable_of_iff isEven_iff

example : Even 2 := by decide
example : Even 4 := by decide
example : Even 6 := by decide
example : Even 100 := by decide
example : Even 1000 := by decide +kernel -- skip the elaborator when evaluating `isEven`.
example : ¬ Even 101 := by decide
example : ∀ n < 10, Even (2 * n) := by decide
example : ∀ n < 10, Even (2 * n) ∧ ¬ Even (2 * n + 1) := by decide
```
an example provided by the eternally helpful Kyle Miller. To me, this code is
astonishingly dense in its interplay between metaprogramming and computation,
especially with some of the details being hidden by the elaboration and
typeclass systems. 

[^live]: Here is a [Lean 4 Web link](https://live.lean-lang.org/#codez=LTAEBUAsEMDsGsDOoAuB7UBpAngGwKagCyAlrgQE6gBmaVADmibCswOahoCuKqkhAAwAm+AMYkRA1NFGtRoOEM7Vq+Cuz4lkotCIBQekKADyAIwBWY3gQBu+XAC5QzIV1kk7ubKBHVm+JQBRO1hQegoAklFoFHw9FzdWO1Bg/FCnADkY0EAkwlAABQo0elAAd34IvVBQAB9QAC81DCdU0IAGKtrQRDdRAH0e0XknQAAiUFgAGhSQ8dzptNAAClCAalAAJgBKAyMifBRoUFt7Jx0AW3oeGJI0WGhcUHwZ0X5ReDU9X2dEVtBM7LyACE0GhcJ06m1QABeAB8qAoXDi1TqAEZoXDqPdEEiuqsNujvr9YDswAItK0pFiMGwPPhkFxkHBnLBXO5bmF1LBxPQCHoAMQvMTwQkhAB0CVkBhQ/Do+DOIrSfRIKlAAG90qAsigAL5/BWhUJQ+GI0CAFMJ5ga/kbTNhOjpYIgUAjZHROgB2mhcWBKlmJG6hckzUIAHgA3HDECQLqAVvbYgAPFBce6gADarVFjSKUwzg36eamACoALru5lOjCQUPM1msdmQUBhiNRkqpwNpIulwxgUqEHRcXBKGDJUrqFCxULSrQ1v3s8LMbkEThcCjY3B2RCffDUfXi32yADkS3yeq1c0KxU2x8AAQR6k9tK+LfKAQII76AUY+XHqxpMCrM8ieywrFsj4alqV5OCe6RGp0ZwxC8sylCQ0rgqAkKwgU16oWiGEvqhsAHsBBJfgRSztrAe61keJ75LeL41uMB7bN2ZSEKU3CDqADKCORlGJFIQgkBEsheHo0r4LK8rkUqKhHuqp4xLqTjkbMRpOia5pElaoA2natyOs66AUGWzAVsgkCdNUEp1paDIaLx1mNuG3Qtmm5HFgovCFk5cL2gZiSutU1SwPgibOA2GGhTIvAkA2FnVB6plFKAVYzmyoQNk2LnRm2PwhJ2JL6syFTIYyoACAAImIEjQKYBBSMwjpwKIcR8og2AsA2P5TFV4hCLVS6LCpsDMY1BxcoQywKSgEGgL1NV1ZNRIQVCnQiH1A34H0aDUH060LQQ227cqO7SSdBhRRcS4tDM6zaTaPjVfol08oQN0LAALPd3j7c98bQFdb0WqAABs32PX1cQvddwMom0kIONaP1PVD/2A3qvxw/D4O/YQKzvBQIUPEYiDwCQJTiY8uC1XQMR0GU/ChE89zJqwsAcGSeVpAIop6NDQMADWw20aKIzpyOQ3zaOvd+szBu+bQ5jMix3d5I04yjUsAzLoxywrSsLCroBq1egDkRKAQu/Ebasxu+K3ixDEj4EAA) which includes some of my own additions.

Let's start with understanding the `isEven`{.lean} function. Intuitively, it
represents a decision procedure that identifies the even numbers. At first
glance, it may seem a bit odd that we are able to use this function in the
proceeding proof to perform induction. Looking at the documentation for
[fun_induction](https://leanprover-community.github.io/mathlib-manual/html-multi//Tactics/All-tactics/#fun_induction)
clears things up a bit. When we define `isEven`{.lean}, we also get an
automatically generated induction principle that intuitively covers the natural
numbers with the two chains $(0 → 2 → 4 → \dots)$ and $(1 → 3 → 5 → \dots)$.
With some renaming for readability, this generated definition is:
```lean
def isEven.induct 
  (P : Nat → Prop) (P₀ : P 0) (P₁ : P 1) (ind : ∀ n, P n → P (n+2)) (n : Nat) : P n := 
  match n with
  | 0 => P₀
  | 1 => P₁
  | n'+2 => ind n' (Nat.ind2 P P₀ P₁ ind n')
```
and in fact we could have essentially the same proof working directly with this
generated principle:
```lean
theorem isEven_iff' {n : Nat} : isEven n = true ↔ Even n := by
  constructor
  · intros h
    induction n using isEven.induct <;> simp [isEven] at * <;> constructor
    next ih => exact ih h
  · intro h; induction h <;> simp [isEven, *]
```
While I don't claim that this sort of induction is unique among proof
assistants, I will emphasize again this code generation is all done via Lean's
metaprogramming system, as part a general ability to derive induction principles
from functions that exhibit structural, well-founded, and mutual recursion. This
is a highly nontrivial development, comprising just over [1700 of Lean
code](https://github.com/leanprover/lean4/blob/master/src/Lean/Meta/Tactic/FunInd.lean).
For some introductory reading, I suggest [Joachim Breitner's blog post on
functional
induction](https://lean-lang.org/blog/2024-5-17-functional-induction/).

The final piece of this puzzle is understanding the examples at the end of the
code snippet. First, we should know that in the background we have the
[Decidable](https://leanprover-community.github.io/mathlib4_docs/Init/Prelude.html#Decidable)
typeclass, which identifies propositions for which we have a procedure that
returns true or false. As booleans have decidable equality (essentially by
definition), we are able to use
[decidable_of_decidable_of_iff](https://leanprover-community.github.io/mathlib4_docs/Init/Core.html#decidable_of_decidable_of_iff)
to transfer the decidability of `isEven`{.lean} to our proposition
`Even`{.lean}. 

Now we are prepared to make use of the
[decide](https://leanprover-community.github.io/mathlib-manual/html-multi//Tactics/Automation/#decide)
tactic. The intuition here is that given an instance of decidability, we should
be able to traverse the corresponding decision procedure, along the way
constructing our proof term. By default, this happens during elaboration, where
the Lean compiler sees the syntax of the `decide`{.lean} tactic, and uses this
to assemble the intended Lean expression. 

Interestingly, this is also an extensible operation. You'll notice that one of
the examples with a larger number uses the configuration `decide
+kernel`{.lean}. This bypasses elaboration and instead uses the Lean
kernel[^kernel] for reduction. This can help performance, but does not introduce
any additional soundness issues, as the kernel is a compilation target that we
already consider as part of our trusted codebase. There is also an option
`decide +native` which uses the Lean compiler to evaluate. This is generally
discouraged, as this does increase the trusted portion of the codebase beyond
its usual bounds, and can introduce subtle bugs. As always, this is all written
as a Lean metaprogram
[Lean.Elab.Tactic.evalDecideCore](https://leanprover-community.github.io/mathlib4_docs/Lean/Elab/Tactic/ElabTerm.html#Lean.Elab.Tactic.evalDecideCore),
where we can examine the different paths taken for the usual elaboration, kernel
reduction, or compiler reduction.

[^kernel]: For more information on the Lean kernel, see [Type Checking in Lean
    4](https://ammkrn.github.io/type_checking_in_lean4/). There are external
    implementations [in Rust](https://github.com/ammkrn/nanoda_lib) and [in
    RPython](https://github.com/Julian/rpylean) to typecheck kernel export files.
