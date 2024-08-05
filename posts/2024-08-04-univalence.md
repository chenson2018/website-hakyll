---
title: 2024 School on Univalent Mathematics
tags: Coq, Univalence, Homotopy Type Theory
---

I spent last week at the [2024 School on Univalent
Mathematics](https://unimath.github.io/minneapolis2024/). For the last couple of
years I've had some interest from a distance in univalence and homotopy type
theory but not taken the plunge, so I was excited that this iteration of the
school was a short trip to Minneapolis away. I had a wonderful time, and felt
that the lectures and accompanying exercises using the Coq library
[UniMath](https://github.com/UniMath/UniMath) were structured in an accessible
way. While "accessible" is certainly a subjective judgement tempered by my
previous background, I felt there was special attention given to making the
school approachable. In particular, while having some previous experience with
Coq definitely helped, this was not assumed at all and several participants were
using a proof assistant for the first time. While there are pros and cons to
this approach, I personally enjoyed the mixture of attendees that this
encouraged.

Given how much I enjoyed the summer school, I thought writing a short blog post
covering the material that I found most interesting would be helpful for
collecting my thoughts. Inherently this is a beginner's perspective, so please
take everything with a grain of salt! If there are any errors that are too
glaring to ignore, please let me know.

## Homotopy

To understand some of the motivation behind univalent foundations, it is helpful
to have some previous knowledge from algebraic topology. The essential idea is
that given some path in a space, we want consider the family of paths that are
continuous deformations from each other. 

It is not too onerous to state this formally, taking definitions from
[Hatcher](https://pi.math.cornell.edu/~hatcher/AT/AT.pdf). Given a topological
space $X$ and ***endpoints*** $x_0, x_1 \in X$, a ***path*** $x_0
\rightsquigarrow x_1$ is a continuous map $f : [0, 1] \to X$ such that $f(0) =
x_0$ and $f(1) = x_1$. Intuitively, we can think of the unit interval as a
parameterization of time in traversing the path. 

A ***homotopy class*** of paths is the equivalence class of paths $f_t : [0, 1]
\to X$, with $t \in [0, 1]$ such that:

* for any paths $f_t$ and $f_{t'}$, there are equal endpoints $f_t(0) =
  f_{t'}(0) = x_0$ and $f_t(1) = f_{t'}(1) = x_1$
* there is a continuous map $F : [0, 1]^2 \to X$, such that $F(s, t) = f_t(s)$

Here $s$ remains our parameter for traversing each path, and the new parameter
$t$ represents sliding along the deformations between each path. The typical
diagram is something like:

```{.tikzpicture style="height: 300px; background-color: white;"}
\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usetikzlibrary{decorations.markings,calc,arrows,calc,shapes,decorations.pathreplacing}
\begin{document}
\begin{tikzpicture}
  [decoration={markings,mark=at position 0.5 with {\arrow{>}}},
   witharrow/.style={postaction={decorate}},
   dot/.style={draw,fill,circle,inner sep=1.5pt,minimum width=0pt}
  ]

  % from https://tex.stackexchange.com/questions/364252/drawing-a-homotopy-between-two-paths  

  % rectangle
  \begin{scope}
     \draw[thick]
       (0,0) coordinate (a1) -- (0,2) coordinate (d1)
       (2,0) coordinate (b1) -- (2,2) coordinate (c1);
     \draw[decorate,decoration={brace,raise=4pt,amplitude=10pt},thick] node[left = 25pt, above = 21pt]{$x_0$} (a1)--(d1) ;
     \draw[decorate,decoration={mirror,brace,raise=4pt,amplitude=10pt},thick] node[right = 80pt, above = 21pt]{$x_1$} (b1)--(c1) ;
     \draw[xstep=2,ystep=1/3] (a1) grid (c1);
     \draw[thick,witharrow] (d1) -- node[above]    {$f_1$}(c1);
     \draw[thick,witharrow] (a1) -- node[below](f1){$f_0$}(b1);
  \end{scope}

  \begin{scope}[shift={(4,1)}]
    \node[dot,label={[left] $x_0$}] (a3) at (0,0) {};
    \node[dot,label={[right]$x_1$}] (b3) at (4,0) {};
    \draw[thick,witharrow] (a3) to[out=50,in=150]node[above]{$f_1$} (b3);
    \foreach \o/\i in {40/160,30/170,20/180,10/190,-10/200}
       \draw (a3) to[out=\o,in=\i]  (b3);
    \draw[thick,witharrow] (a3) to[out=-20,in=-130]node[below]{$f_0$} (b3);
  \end{scope}

  % connecting arrows
  \draw[-stealth,shorten >=4mm] (1, 3) -- node[above]{$F(s, t) : [0, 1]^2 \to X$} (6.5, 3);
\end{tikzpicture}
\end{document}
```

We can consider a special restriction of paths with the same starting and ending
points, so that $f(0) = f(1) = x_0$. These are called ***loops***, with some given
***basepoint*** $x_0 \in X$. The corresponding homotopy class, denoted by
$\pi_1(X, x_0)$, actually forms a group called the ***fundamental group***[^base_note]. Here
the group action is the composition of paths, and inverses correspond to
traversing the path in the opposite direction.

[^base_note]: When our space is simply connected the choice of basepoint is irrelevant
      up to isomorphism, so we can simply write $\pi_1(X)$.

We can view the fundamental group as a way to study topological spaces in terms
of groups. If we consider ***pointed topological spaces***, simply a space $X$
with some identified basepoint $x_0$, then in category theoretic terms the
fundamental group is a functor $\pi_1 \colon \mathbf{Top}^{* \downarrow} \to
\mathbf{Grp}$ from the category of pointed topological spaces to the category of
groups. This idea of forming an algebraic viewpoint of a topological space is a
powerful idea that lies at the essence of algebraic topology.

The typical first example of actually computing a fundamental group is for the
circle $S^1$, for which $\pi_1(S^1)$ is isomorphic to the additive group of
integers $(\mathbb{Z}, +)$. Intuitively we can see this by viewing each
traversal or inverse traversal of the circle as the successor or predecessor
function on the integers.

## Univalent Foundations

So, what is the relevance of all this to type theory? A key idea will be that we
would like to replace the usual notion of equality $x = y$ with the notion of
there being a path $x \rightsquigarrow y$. In some senses this does behave like
classical equality. Paths are an equivalence relation, and we can rewrite with
paths in the sense that there is a function:

$$
\operatorname{transport} : \prod (X \, Y : \operatorname{Type}) (P : X \to Y) (x \, x' : X), \, (x \rightsquigarrow x') \to P(x) \to P(x')
$$

Borrowing from algebraic topology nomenclature, we refer to this as operating
over the ***fibers*** of our space. Diagrammatically we have something like:

```{.tikzpicture style="height: 300px; background-color: white;"}
\documentclass[border=2mm]{standalone}
\usepackage{tikz}
\usetikzlibrary{decorations.markings,calc,arrows,calc,shapes,decorations.pathreplacing,decorations.pathmorphing}
\begin{document}
\begin{tikzpicture}[y=.5cm]
   \foreach \dot in {1, ..., 13}
     \draw coordinate (n-\dot) at ([xslant=.3333] xyz cs: x={mod(\dot,5)}, y={int(\dot/5)});
   \draw[fill=white] (n-7) node[below = 15pt, right = 30pt]{$X$} circle[radius=2.4];
   \node[outer sep=0pt,circle, fill,inner sep=1.5pt,label={left:$x$}] (a) at (n-1) {};
   \node[outer sep=0pt,circle, fill,inner sep=1.5pt,label={right:$x'$}] (b) at (n-13) {};
   \node[outer sep=0pt,circle, fill,inner sep=1.5pt,label={left:$P(x)$}] (c) at (n-1|-0,4) {};
   \node[outer sep=0pt,circle, fill,inner sep=1.5pt,label={right:$P(x')$}] (d) at (n-13|-0,4.5) {};
   \draw (n-1) -- (n-1|-0,5);
   \draw (n-13) -- (n-13|-0,5);
   \draw [->,
        line join=round,
        decorate, decoration={
            zigzag,
            segment length=4,
            amplitude=.9,post=lineto,
            post length=2pt
        }
    ] (a) -- (b);
\end{tikzpicture}
\end{document}
```

The difference is that we potentially can have much more structure on this
identity type than we usual equality. We can have paths not just between points,
but between paths, corresponding to the idea of homotopy above. The difference
between equivalence in usual type theories and univalent foundations lies in
what axioms we adopt regarding how much structure is allowed in these higher
order identity types.

One approach, for instance in Lean 4, is to add as an axiom[^axiom] that all proofs of
identity are equal, that is:

[^axiom]: According to [this Zulip thread](https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/UIP.20in.20Lean.20source.20code), this is actually built into Lean's type checking itself.

$$ \prod (X : \operatorname{Type}) (x \, x' : X) (p \, q : x \rightsquigarrow x'), \, p \rightsquigarrow q  $$

which is known as the ***uniqueness of identity proofs*** (UIP). In univalent
foundations, this notion is more refined and requires a bit of setup to
describe. First we have ***contractible*** types, defined as satisfying

$$
\operatorname{isContr}(X : \operatorname{Type}) := \sum_{x : X} \, \prod_{x' : X} x \rightsquigarrow x'
$$

which intuitively identifies types that are singletons, like the unit type.
Stepping up one level, we define the following notion of equivalence as the
existence of contractible fibers

$$
    \operatorname{isequiv}(f : X \to Y) := \prod_{y : Y} \operatorname{isContr} \left (\sum_{x : X} f(x) \rightsquigarrow y \right)
$$

and write 

$$
    X \simeq Y := \sum_{f : X \to Y} \operatorname{isequiv}(f)
$$

The ***univalence axiom*** is the statement that 

$$
    \prod_{X \, Y : \operatorname{Type} } (X \rightsquigarrow Y ) \simeq (X \simeq Y)
$$

which intuitively allows us to transform an isomorphism between types into an
(path) equality on these types. Taking this as an axiom, a choice inconsistent
with UIP, is a core idea of univalent foundations.

The practical appeal is not too difficult to see. It is not uncommon to have
multiple implementations of a type due to some low-level practicalities, often
resulting in a painful duplication of proofs that are essentially the same. One
potential application of univalence is that given isomorphic types that some of
this could be automated via ***proof transfer***[^auto].

[^auto]: In fact, there is potential for proof transfer [even without
    isomorphism](https://dl.acm.org/doi/10.1145/3434293) and the notion of
    [univalent parametricity](https://dl.acm.org/doi/10.1145/3429979). See
    [Trocq](https://github.com/coq-community/trocq) for a recent implementation
    that builds on some of these ideas.

## Homotopy Levels

The immediate question that comes to mind is how much additional structure is
added by not collapsing all identity proofs? For any given type $X$, we have an
infinite family of identity types

$$
\rightsquigarrow_X, \, \rightsquigarrow_{\rightsquigarrow_X}, \rightsquigarrow_{\rightsquigarrow_{\rightsquigarrow_X}}, \dots
$$

to consider. We essentially are looking for a notion of how "twisty" a given
space can be, and capture this idea formally with the idea of ***homotopy
level*** (h-level) that counts how far down this chain of identity types we must traverse
until we reach a contractible (singleton) type. Formally we have the definition:

\begin{align}
\operatorname{isofhlevel}         & : \mathbb{N} \to \operatorname{Type} \to \operatorname{Type} \\
\operatorname{isofhlevel} (0, X)     & := \operatorname{isContr}(X) \\
\operatorname{isofhlevel} (n + 1, X) & := \prod_{x \, x' : X} \, \operatorname{isofhlevel} (n, x \rightsquigarrow_X x')
\end{align}

and at the bottom of this hierarchy we provide some suggestive names:

\begin{align}
\operatorname{isProp}(X : \operatorname{Type})  & := \operatorname{isofhlevel} (1, X) \\
                                                & := \prod_{x \, x' : X} \, \operatorname{isContr(x \rightsquigarrow_X x')} \\
\\
\operatorname{Prop} & := \sum_{X : \operatorname{Type}} \operatorname{isProp}(X)
\end{align}

and

\begin{align}
\operatorname{isSet }(X : \operatorname{Type})  & := \operatorname{isofhlevel} (2, X) \\
                                                & := \prod_{x \, x' : X} \, \operatorname{isProp (x \rightsquigarrow_X x')}
\\
\operatorname{Set} & := \sum_{X : \operatorname{Type}} \operatorname{isSet}(X)
\end{align}

For clarity, I will refer to these as h-propositions and h-sets respectively,
though they are often (somewhat confusingly) mentioned without the h- prefix.
This notion of h-set by definition is exactly corresponding to identifying types
that satisfy the uniqueness of identity proofs.

Note that despite the names "Prop" and "Set", these are not universes in the
sense the terminology is used in proof assistants! This remains a bit of a point
of confusion for myself, so I'll refrain from commenting too much on this point.
They *do* seem to be related concepts, as our univalent foundations has an
analog of the Curry-Howard isomorphism where terms of h-proposition types are
interpreted as proofs. My understanding however is that these remain somewhat
orthogonal considerations.

So where exactly does this show up? There are a few theorems that I think give
some intuition for where h-levels are relevant in types that are familiar. The
first is that any type with decidable equality, such as the natural numbers or
booleans, is an h-set. Note that the converse is not necessarily true, with the
most instructive example being Prop, which is an h-set but certainly not
decidable.

TODO : Coq syntax highlighting

Another instructive example is found by looking at the definition of a category
in the UniMath library:

```haskell
Definition has_homsets (C : precategory_ob_mor) : UU := ∏ a b : C, isaset (a --> b).

Definition category := ∑ C:precategory, has_homsets C.
```

We'll gloss over the exact definition of a precategory. It is exactly what you
would expect, just the standard rules for identity and composition. The
interesting piece here is the requirement that the morphisms form h-sets. After
a bit of thought and some explanations from Benedikt Ahrens and Niels van der
Weide during the summer school, this restriction makes sense to me as a way of
ensuring that our definition that corresponds to a category. My intuition is
roughly that without this restriction we would have something that looks more
like a higher category. Without the uniqueness of identity proofs that we have
at the h-set level, our proof obligations for the category laws could become in
a sense incompatible with morphisms of higher h-levels.

In the opposite direction of identifying types which are not h-sets, we have a
few intuitive examples such as Set and Type. In the following section we will
see that $S^1$ is a perhaps more concrete example of a type with an h-level
higher than h-sets.

## Synthetic Homotopy Theory

So far we've developed a good deal of theory, which at least for me has
intrinsic mathematical value, but what are the applications? The most immediate
answer is that we can do homotopy theory with univalent foundations inside a
proof assistant! In fact, computing the fundamental group of the circle has
become somewhat of a standard first exercise in various proof assistants, as
seen in [this Arend
tutorial](https://arend-lang.github.io/documentation/tutorial/PartII/hits.html),
the Agda [HoTT Game](https://homotopytypetheory.org/2021/12/01/the-hott-game/),
or [this
exercise](https://github.com/UniMath/Schools/blob/master/2024-07-Minneapolis/7_Synthetic-Homotopy-Theory/circle_exercises.v)
from the School on Univalent Mathematics.

For anyone who has ever worked with real numbers, continuous maps, or
topological spaces in a proof assistant, you might have the inclination to
expect some gritty details in computing something like the fundamental group.
The trick here is that we will be approaching this from the ***synthetic***
point of view, which in this context means that we will rely on the fact that we
are working within a type theory equipped with a native notion of paths as
its definition of equality, and exploit this in our definition of a circle[^plato]. Thus
we can forego even defining real numbers or topological spaces and essentially
do homotopy theory from an axiomatic point of view, similarly to the classical
development of geometry.

[^plato]: There is a bit of a philosophical question here of how exactly this
    corresponds to the "platonic" notion of homotopy theory from standard
    mathematics. However, I'd argue that this is inherent to working in a proof
    assistant! See [this Stack Exchange answer by Mike
    Shulman](https://proofassistants.stackexchange.com/a/201/588) for some
    relevant discussion. 

What follows is my solution to the final exercises from the summer school, that
$(base \rightsquigarrow_{S^1} base) \rightsquigarrow \mathbb{Z}$. Philosophical
considerations aside, the definition of $S^1$ that we would like to work with is
something like

```haskell
inductive S1
  | base
  | loop : base = base
```

Our issue is that this is not a legal definition in Coq, which does not have
higher inductive types. The approach taken in the summer school was to simply
state as an axiom the `loop` piece, along with the recursion and inductive
principles.

TODO : code here

TODO : note about PAs with HI types

TODO: the rest of the proof....
TODO: et this working with Alectryon

## References

TODO: organize this lol, maybe integrate into the article...

https://github.com/JetBrains/arend-lib/blob/master/src/Homotopy/Sphere/Circle.ard

https://arend-lang.github.io/documentation/language-reference/definitions/hits.html

https://arend-lang.github.io/documentation/tutorial/PartII/hits.html

https://unimath.github.io/Schools/#school-on-univalent-mathematics-minneapolis-2024

https://ncatlab.org/nlab/show/pointed+topological+space

https://ncatlab.org/nlab/show/comma+category

https://ncatlab.org/nlab/show/h-set

https://ncatlab.org/nlab/show/fundamental+group

https://ncatlab.org/nlab/show/synthetic+homotopy+theory

https://www.ams.org/notices/201309/rnoti-p1164.pdf

https://arend-lang.github.io/documentation/language-reference/definitions/hits.html

[Algebraic Topology](https://pi.math.cornell.edu/~hatcher/AT/AT.pdf) \

