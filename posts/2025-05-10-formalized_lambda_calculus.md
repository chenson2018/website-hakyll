---
title: Beginner Resources for Formalizing Lambda Calculi
tags: Lambda Calculus, Proof Assistants, Lean, Coq, Agda
---

This started as a Twiter reply to [@slimjimmy_dev](https://x.com/slimjimmy_dev)
asking for some resources to help understand my Lean formalization of the
untyped lambda calculus[^catsem], but quickly grew long enough that I decided to
make it into a post. Formalizing metatheory is a topic that is interesting to
beginner and experienced proof assistant users alike, but rife with tricky
technical issues that can dissuade one from making much progress. I hope by
collecting some resources here that I can provide a bit of help to those looking
for where to get started.

[^catsem]: Now that the groundwork is laid I should be able tackle the
    categorical semantics of the untyped lambda calculus in Lean next! I claim
    no originality for the confluence/progress pieces, they are an amalgamation
    of the resources listed in this article.

The first tip that I would give is that it is very useful to be "multilingual"
with respect to proof assistants. Lean and Rocq are close enough that you can
reasonably transfer knowledge between the two, but it is immensely helpful to
also have enough working knowledge of Agda to be able to at least read along and
translate other formalizations into your proof assistant of choice.

The first resource I'll mention is *[Software Foundations, Volume 2: Programming
Language
Foundations](https://softwarefoundations.cis.upenn.edu/plf-current/toc.html)*.
As with the entirety of the series, it is carefully structured and accompanied
with well-written explanations that make it a good starting point. It walks
through a formalization of the simply typed lambda calculus (STLC), as well as a
few extensions. One of the reasons that this is a useful and interesting
resource from the pedagogy perspective is its treatment of variable binding and
substitution. It takes the approach of treating bindings as strings, but
combines this with some simplifying assumptions that make substitution not as
difficult to work with. It also has the benefit of explicitly referring to
chapters of *Types and Programming Languages*, which can be helpful for learning
to connect informal and formal definitions.

In general, this is not the approach that formalizations take. To understand
why, consider the terms $(\lambda x \, . \, x)$ and $(\lambda y \, . \, y)$.
These are $\alpha$-equivalent, meaning they are the same up to a consistent
replacement of variable names. However, from the perspective of a formalization
that treats variable bindings as strings, they are not *definitionally equal*
terms from the perspective of the proof assistant. The usual approach here is to
use *de Bruijn indices*, where a variable is instead represented by a natural
number that indicates how far away its binding is. Now, the two terms above both
become represented by  $(\lambda \, . \, 0)$, and we can use the proof
assistant's notion of equality. Another issue (in typed systems) is the idea of
typing being *extrinsic* or *intrinsic*.  In Software Foundations types and
terms are defined separately (extrinsic), while another approach would be to
find a way to define a single inductive type that only permits well-typed terms
(intrinsic).

A resource that addresses these two ideas is Part 2 of [Programming Language
Foundations in Agda](https://plfa.github.io/). It covers some of the same ground
as Software Foundations, but takes the approach of formalizing STLC using both
de Bruijn indices and intrinsic typing. Another valuable contribution of PLFA is
that it also formalizes the untyped lambda calculus and its property of
*confluence*, the idea that we can pick any order of reductions and eventually
converge back to the same term.

In practice however, it can be very painful to work with de Bruijn indices. When
we construct terms and perform substitutions, we are required to "shift" these
indices, and lemmas related to the behavior of this shifting can be quite
cumbersome. An alternative approach is to define the syntax of our lambda
calculus terms to differentiate between bound and free variables. We still must
prove some lemmas regarding how different operations on free and bound variables
interact, but this is much less arduous. [This Rocq
tutorial](https://www.cis.upenn.edu/~plclub/popl08-tutorial/code/) from the Penn
PL Club serves as a great introduction to this approach, including both simple
types and System F$_{<:}$.

Another tip is to scour GitHub for as many examples of formalizations as
possible, as it is helpful to see the slight variations in formalization and
proof structure. Eventually, as you grow more comfortable, you will be able to
mix and match according to your needs and preferences. Here are additional repos
and resources that I have found helpful:

&nbsp;

| Explanation  | Link | 
| ---  | --- |
| My own repo of miscellaneous Lean formalizations that includes the untyped lambda calculus in both the de Bruijn index and locally nameless approaches, each with a proof of confluence  | <https://github.com/chenson2018/LeanScratch> |
| A Rocq formalization of the untyped lambda calculus that I found useful when I wanted to prove some lemmas about confluence over generalized relations  | <https://gist.github.com/siraben/ee3f16bf501ab7ecb49d63ecd3a2d2b1> |
| A Lean implementation of part 2 and 3 of PLFA  | <https://github.com/rami3l/PLFaLean> |
| A Lean formalization of STLC that takes the locally nameless approach  | <https://github.com/ElifUskuplu/Stlc_deBruijn> |
| Agda and Isabelle formalizations of the λ-Y calculus  | <https://github.com/goodlyrottenapple/lamYcalc?tab=readme-ov-file> |
| A Rocq implementation of System F that includes a proof of parametricity  | <https://github.com/Lysxia/system-F> |
| Several nice Rocq formalizations, including Fω and STLC extended with pairs  | <https://github.com/yiyunliu> |
| An Agda formalization of System F with iso-recursive types  | <https://github.com/sstucki/system-f-agda> |
| An Agda formalization of System F$_\omega$$_\mu$ using intrinsic typing  | [System F in Agda, for Fun and Profit](https://homepages.inf.ed.ac.uk/wadler/papers/mpc-2019/system-f-in-agda.pdf) |
| Several formalizations of System F$_{<:}$ across different proof assistants. This is a bit older (early 2000s) but I think is still valuable for providing so many examples of different approaches.  | [POPLmark Challenge](https://www.seas.upenn.edu/~plclub/poplmark/) |
| Several formalizations of strong normalization for STLC | [POPLmark Reloaded](https://poplmark-reloaded.github.io/) |
| The classic text by Barendregt on the untyped lambda calculus. While yellow math books have a reputation of instilling fear and bewilderment in their readers, I think this an excellently written book. It is definitely not a beginner text, but I highly recommend it after you feel comfortable with the other resources.  | [The Lambda Calculus: Its Syntax and Semantics](https://www.amazon.com/Calculus-Semantics-Studies-Foundations-Mathematics/dp/0444875085) |
