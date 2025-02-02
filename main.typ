#import "@preview/touying:0.5.3": *
#import themes.metropolis: *
#import "@preview/cetz:0.3.1"
#import "@preview/fletcher:0.5.2" as fletcher: node, edge
#import "@preview/ctheorems:1.1.3": *
#import "@preview/curryst:0.3.0": rule, proof-tree
#import "@preview/numbly:0.1.0": numbly
#import "./catt.typ": *
#import "./dtt.typ": *
#show: metropolis-theme.with(aspect-ratio: "16-9")

// Pdfpc configuration
// typst query --root . ./example.typ --field value --one "<pdfpc-file>" > ./example.pdfpc
#pdfpc.config(
  duration-minutes: 30,
  start-time: datetime(hour: 14, minute: 10, second: 0),
  end-time: datetime(hour: 14, minute: 40, second: 0),
  last-minutes: 5,
  note-font-size: 12,
  disable-markdown: false,
  default-transition: (
    type: "push",
    duration-seconds: 2,
    angle: ltr,
    alignment: "vertical",
    direction: "inward",
  ),
)

// Custom Table Styling
#let toptable = (..content) => {
  table(
    fill: (x, y) => if y == 0 {
      silver
    },
    stroke: (x, y) => if y == 0 {
      (
        top: (thickness: 1pt, paint: silver),
      )
    } else if y > 0 {
      (
        top: (thickness: 1pt, paint: silver),
        left: (thickness: 1pt, paint: silver),
        right: (thickness: 1pt, paint: silver),
        bottom: (thickness: 1pt, paint: silver),
      )
    },
    inset: 7pt,
    ..content,
  )
}

#let lefttable = (..content) => {
  table(
    fill: (x, y) => if x == 0 {
      silver
    },
    stroke: (x, y) => if x == 0 {
      (
        right: (thickness: 1pt, paint: silver),
      )
    } else if x > 0 {
      (
        top: (thickness: 1pt, paint: silver),
        left: (thickness: 1pt, paint: silver),
        right: (thickness: 1pt, paint: silver),
        bottom: (thickness: 1pt, paint: silver),
      )
    },
    inset: 7pt,
    ..content
  )
}


// Theorems configuration by ctheorems
#show: thmrules.with(qed-symbol: $square$)
#let theorem = thmbox("theorem", "Theorem", fill: rgb("#eeffee"))
#let corollary = thmplain(
  "corollary",
  "Corollary",
  base: "theorem",
  titlefmt: strong,
)
#let definition = thmbox("definition", "Definition", inset: (x: 1.2em, top: 1em))
#let example = thmbox("example", "example", fill: rgb("#eeeeee")).with(numbering: none)
#let proof = thmproof("proof", "Proof")

#let boxify = content => box(fill: silver, inset: 0.5em, [#content])

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  // config-common(handout: true),
  config-info(
    title: [Equality and Isomorphisms in Type Theory],
    subtitle: [HoTT to Parametric Univalence and Beyond; `Trocq` @trocq],
    author: [Abdul Haliq],
    date: datetime.today(),
    institution: text(
            size: 1.2em,
            font: ("AU Passata"),
          )[#upper[Aarhus Universitet]],
  ),
)

#set heading(numbering: numbly("{1}.", default: "1.1"))

#title-slide()

== Motivation <touying:hidden>

- *Homotopy Type Theory*; the univalence axiom logically performs proof transfer, but not computationally #pause
- *Higher Observational Type Theory*: we use parametericity translations to define univalence symmetrically using relations expressed in the calculus itself #pause
- *General Proof Transfer*: allows proof transfer to be done beyond type equivalence, e.g. the type of $[0,+ infinity)$ and $[0, +infinity]$ where the transfer for $+infinity$ have to be manually managed, but with `Trocq` is automated

#pagebreak()

- Recall our running example of proof transfer
- Let $N$ be the type of big unsigned integers in binary form
- Let $bb(N)$ be the inductive type of peano natural numbers in terms of zero and successor #pause
- we have an isomorphism between them, i.e. a function from $N$ to $bb(N)$ and back
$
  (f,g, \_) : N iso bb(N)
$
#pause
- we have constructed a function $"isEven"_bb(N)$ for natural numbers
$
  "isEven"_bb(N) zero &= btrue \
  "isEven"_bb(N) (succ n) &= not ("isEven"_bb(N) n)
$
#pause
- we can solve this with a computational proof transfer under type equivalence

#pagebreak()

- what about types with weaker relationships than type equivalence?
- Let $Q = "int" times "int"$
- Let `rat` be the type of rational numbers
```Coq
Record rat : Set := Rat {
  valq : int * int;
  _ : (0 < valq.2) && coprime `|valq.1| `|valq.2|
}.
```#pause
- we clearly have a map `valq` from `rat` to `Q`
- but not all possible `Q` can be mapped to `rat`
```Coq
Definition Qint_to_rat (r : int * int) : option rat :=
  if r.2 != 0 then Some (r.1%:Q / r.2%:Q) else None.
```#pause
- thus type equivalence is too strong for proof transfer between these types

== Outline <touying:hidden>

#components.adaptive-columns(outline(title: none, indent: 1em))

= Recap on HoTT

== Dependent Type Theory

- *Substitution Calculus*: dependent types are expressed with judgements of terms, types and substitutions in a context which are a list of dependent terms, with primitive substitutions $id, sp, sq$ and context extension $gamma. a$ #pause
- *Mapping In Types*: are type formers $Upsilon$ such as $Eq$ are defined by the isomorphism
$
  Tm(Gamma, Upsilon) iso Y
$
- *Mapping Out Types*: are type formers $Upsilon$ such as $Id$ are defined by the isomorphism
$
  { c in Tm(Gamma. Upsilon, C) | rec } iso { star }
$
- *Univalence*: identification of types are equivalent to their equivalence
$
  ua : Id(UU, A, B) equiv (A equiv B)
$

== Univalence Generally

$
  {attach(arrow.t arrow.b, br: B) in Tm(Gamma. Id(U, A, B), C) | u } iso {star}
$
- we want $attach(arrow.t, br: B)$ to map proofs in $A$ to $B$, and $attach(arrow.b, br: B)$ vice versa #pause
$
  u : C(Id(U,A,B),C(A,B))
$
- $U$ is univalent when $attach(arrow.t arrow.b, br: B)$ induces $u$ which is also a term of $C$
#figure(
  diagram(
    cell-size: 10mm,
    $
      A
  #edge("rr", $attach(arrow.t arrow.b, br: B)$, "<-->", shift: 0pt, label-anchor: "south", label-sep: 0em, bend: 30deg)
  #edge("rr", $p: Id_U$, "->", shift: 0pt, label-anchor: "north", label-sep: 0em, bend: -30deg)
  #edge((1,-0.25),(1,0.25), $u$, "<-->")
  & &
B
    $,
  ),
)
#pause
- naively we let $C$ be isomorphisms or bimaps, but this is too strong that it only works on a subtype of $UU$ called $HProp$; homotopy propositions, types with only one term @prop
$
  HProp = Sigma(A : UU, Pi(x : A, Pi(y : A, Id(A,x,y))))
$

== Prop Univalence

$
  "propUnivalence" : Id(HProp,A,B) iso (A iso B) #pause \
$
$
  C(A,B) = A iso B &= Sigma((f,g) : A <-> B, "areIso"(f,g)) \ #pause
  A <-> B &= (A -> B) times (B -> A) \ #pause
  "areIso"(f,g) &= Id(A -> A, g comp f, id) times Id(B -> B, f comp g, id) #pause
$
$
  "propUnivalence" = lambda p . (subst id p, subst id (sym p), ...)
$
#align(center)[_think both types need to have the same amount of terms; this is too restrictive_]

#figure(
  cetz.canvas({
    import cetz.draw: *
    scale(x: 60%, y: 60%)
    circle((0, 0), name: "L")
    circle((3, 0), name: "R")
    circle((1.5, 0), radius: (3, 2), name: "HProp")
    fill(black)
    circle((0, 0), radius: 0.15, name: "a1")
    circle((3, 0), radius: 0.15, name: "b1")
    fill(black)
    content("L.north", [$A$], anchor: "south")
    content("R.north", [$B$], anchor: "south")
    content("HProp.north", $HProp$, anchor: "south")
    stroke(black)
    stroke((paint: purple, dash: "dashed"))
    line("a1.east", "b1.west", name: "line1", mark: (end: ">", start: ">"))
  }),
)

== Full Univalence

$
  ua : Id(UU, A, B) equiv (A equiv B) #pause
$
$
  C(A,B) = A equiv B &= Sigma(f : A -> B, "isEquiv"(f)) \ #pause
  "isEquiv"(f) &= Pi(b : B, "isContr"("fib"(f,b))) \ #pause
  "isContr"(X) &= Sigma(x : X, Pi(y : X, Id(X, x, y))) \ #pause
  "fib"(f,b) &= Sigma(a : A, Id(B, f#h(0.25em) a, b))
$
#meanwhile
#align(center)[_think both types need to have the same amount of "covers" of terms_]
#figure(
  toptable(
    columns: 4,
    align: (center + horizon, center + horizon, center + horizon, center + horizon),
    [fibre],
    [contractible],
    [contractible fibre],
    [univalence],
    figure(
      cetz.canvas({
        import cetz.draw: *
        scale(x: 60%, y: 60%)
        circle((0, 0), radius: (1, 1.5), name: "L")
        circle((3, 0), radius: (1, 1.5), name: "R")
        fill(black)
        circle((0, 1), radius: 0.15, name: "a1")
        circle((0, 0.25), radius: 0.15, name: "a2")
        circle((0, -0.5), radius: 0.15, name: "a3")
        circle((0, -1.2), radius: 0.15, name: "a4")
        circle((2.75, 0.8), radius: 0.15, name: "b1")
        circle((2.75, 0), radius: 0.15, name: "b2")
        circle((2.75, -0.8), radius: 0.15, name: "b3")
        circle((3.75, 0), radius: 0.15, name: "b4")
        circle((3.2, -1), radius: 0.15, name: "b5")
        fill(black)
        content("L.north", [$A$], anchor: "south")
        content("R.north", [$B$], anchor: "south")
        stroke(black)
        stroke((paint: gray, dash: "dotted"))
        line("a1.east", "b1.west", name: "line1", mark: (end: ">"))
        content("line1.mid", $f$, anchor: "south")
        line("a2.east", "b2.west", name: "line2", mark: (end: ">"))
        line("a3.east", "b3.west", name: "line3", mark: (end: ">"))
        stroke((paint: purple, dash: "solid"))
        line("b1.east", "b4.north", name: "line4", mark: (end: ">"))
        line("b2.east", "b4.west", name: "line5", mark: (end: ">"))
        line("b3.east", "b4.south", name: "line6", mark: (end: ">"))
      }),
    ),
    figure(
      cetz.canvas({
        import cetz.draw: *
        scale(x: 60%, y: 60%)
        circle((0, 0), radius: (1.5, 1.5), name: "X")
        content("X.north", $X$, anchor: "south")
        fill(black)
        circle((0, 0.75), radius: 0.15, name: "x1")
        content("x1.east", $x$, anchor: "west")
        circle((-0.75, -0.5), radius: 0.15, name: "x2")
        circle((0, -0.75), radius: 0.15, name: "x3")
        circle((0.75, -0.5), radius: 0.15, name: "x4")

        stroke((paint: purple, dash: "solid"))
        line("x1.south", "x2.north", name: "line1", mark: (end: ">"))
        line("x1.south", "x3.north", name: "line1", mark: (end: ">"))
        line("x1.south", "x4.north", name: "line1", mark: (end: ">"))
      }),
    ),
    figure(
      cetz.canvas({
        import cetz.draw: *
        scale(x: 60%, y: 60%)
        circle((0, 0), radius: (1, 1.5), name: "L")
        circle((3, 0), radius: (1, 1.5), name: "R")
        fill(black)
        circle((0, 0.8), radius: 0.15, name: "a1")
        fill(purple)
        stroke(purple)
        circle((0, 0), radius: 0.15, name: "a2")
        fill(black)
        stroke(black)
        circle((0, -0.8), radius: 0.15, name: "a3")
        circle((2.75, 0.8), radius: 0.15, name: "b1")
        circle((2.75, 0), radius: 0.15, name: "b2")
        circle((2.75, -0.8), radius: 0.15, name: "b3")
        circle((3.75, 0), radius: 0.15, name: "b4")
        fill(black)
        content("L.north", [$A$], anchor: "south")
        content("R.north", [$B$], anchor: "south")
        stroke(black)
        stroke((paint: gray, dash: "dotted"))
        line("a1.east", "b1.west", name: "line1", mark: (end: ">"))
        content("line1.mid", $f$, anchor: "south")
        line("a2.east", "b2.west", name: "line2", mark: (end: ">"))
        line("a3.east", "b3.west", name: "line3", mark: (end: ">"))
        stroke((paint: black, dash: "solid"))
        line("b1.east", "b4.north", name: "line4", mark: (end: ">"))
        line("b2.east", "b4.west", name: "line5", mark: (end: ">"))
        line("b3.east", "b4.south", name: "line6", mark: (end: ">"))
        stroke((paint: purple, dash: "solid"))
        line("a2.north", "a1.south", mark: (end: ">"))
        line("a2.south", "a3.north", mark: (end: ">"))
        line("line5.25%", "line4.25%", mark: (end: ">"))
        line("line5.25%", "line6.25%", mark: (end: ">"))
      }),
    ),
    figure(
      cetz.canvas({
        import cetz.draw: *
        scale(x: 60%, y: 60%)
        circle((0, 0), radius: (1, 1.5), name: "L")
        circle((3, 0), radius: (1, 1.5), name: "R")
        circle((1.5, 0), radius: (3, 2.5), name: "UU")
        circle((0, 0), radius: (0.3, 0.5), name: "a1")
        circle((-0.2, 0.5), radius: (0.3, 0.5), name: "a2")
        circle((0, -0.7), radius: (0.3, 0.5), name: "a3")
        circle((3, 0), radius: (0.3, 0.5), name: "b1")
        circle((2.7, 0.8), radius: (0.3, 0.5), name: "b2")
        circle((3.1, -0.5), radius: (0.3, 0.5), name: "b3")
        fill(black)
        content("L.north", [$A$], anchor: "south")
        content("R.north", [$B$], anchor: "south")
        content("UU.north", $UU$, anchor: "south")
        stroke(black)
        stroke((paint: purple, dash: "dashed"))
        line("a1.east", "b1.west", name: "line1", mark: (end: ">"))
        line("a2.east", "b2.west", name: "line2", mark: (end: ">"))
        line("a3.east", "b3.west", name: "line3", mark: (end: ">"))
      }),
    ),
  ),
)

== Equiv and Iso

*Moreover*
- $"isIso"(f) -> "isEquiv"(f)$
- $"isEquiv"(f) : HProp$
- proof in @daniel chapter on Univalence #pause
*Cubical Type Theory (CTT)*
- includes a pretype interval $bb(I)$ where $Id$ depends on, univalence here is computational
- in this presentation we define univalence with parametricity translations instead

= Parametricity Translation

== Proof Transfer In General

- given a type former $V : A -> UU$
- given the relations $R_T : A -> B -> UU$ and $R_UU : UU -> UU -> UU$
- we synthesize $W : B -> UU$ such that the following holds
$
  Gamma hy w : Pi(a : A, b : B, R_T (a, b) -> R_UU (V a, W b))
$
#align(center)[_think if relation between terms of $A$ and $B$ holds, then types indexed on them; $V$ and $W$ also holds in relation_]

== Example $N, bb(N)$

$
  Gamma hy w : Pi(a : A, b : B, R_T (a, b) -> R_UU (V a, W b))
$
*different types*; _type equivalence_
#figure(
  grid(
    columns: 2,
    align: (center + horizon, center + horizon),
    $
      A =& bb(N) times (bb(N) -> bb(N)) \
      V =& Pi(X : bb(N), Pi(P : bb(N) -> UU, \
  &P(X.1) -> \
  &Pi(n : bb(N), P(n) -> P(X.2(n)) -> \
  &Pi(n : bb(N), P(n)))))
    $,
    pause,

    $
      B =& N times (N -> N) \
      W =& Pi(X : N, Pi(P : N -> UU, \
  &P(X.1) -> \
  &Pi(n : N, P(n) -> P(X.2(n)) -> \
  &Pi(n : N, P(n)))))
    $,
  ),
)
#align(center)[_$R_T$ is a type equivalence between $bb(N)$ and $N$ transporting $V(a)$ to $W(b)$ along this equivalence_]

#pagebreak()

$
  Gamma hy w : Pi(a : A, b : B, R_T (a, b) -> R_UU (V a, W b))
$
*common interface*; _representation independence_
$
  A = B =& Sigma(upright(N) : UU, upright(N) times (upright(N) -> upright(N))) \
  V = W =& Pi(X : A, Pi(P : X.1 -> UU, \
  &P(X.2) -> \
  &Pi(n : X.1, P(n) -> P(X.3(n))) -> \
  &Pi(n : X.1, P(n))))
$
#align(center)[_$R_T$ characterizes isomorphic instances of the structure_]
#pagebreak()

$
  Gamma hy w : Pi(a : A, b : B, R_T (a, b) -> R_UU (V a, W b))
$
*trivial*
$
  W =& Pi(X : N, Pi(P : bb(N) -> UU, \
  &P(attach(arrow.t, br: bb(N))(X.1)) -> \
  &Pi(n : bb(N), P(n) -> P(attach(arrow.t, br: bb(N))(X.2(attach(arrow.b, br: bb(N))(n)))) -> \
  &Pi(n : bb(N), P(n)))))
$
- remember $W$ is indexed on $N$
- composing with $attach(arrow.t arrow.b, br: bb(N))$ can only propagate structural arguments
- this ignores additional proofs of program equivalences
- this was the concluding solution we had in the previous presentation

== Proof Transfer Strategies

- Proof transfer automation consist of a meta-program that computes $W$ and $w$ by induction on the structure of $V$ #pause
- proof transfer strategies differ by the relations they can express #pause
- *generalized rewriting* provide support to setoid based formulations for homogeneous functional relations; $A=B$ #pause
- *`CoqEAL`*@refinements library provide support to refinements specialized to heterogeneous functional relations; $A != B$, in quantifier free type formers #pause
- *`Trocq`* calculus provide support to heterogeneous functional relations in dependent type formers

== More on `CoqEAL`

- recall our example on $Q$ and `rat` where `Qint_to_rat : (int * int) -> option rat`
- the functional relation is then (where `=` is `Id`)
```Coq
Definition RRat : rat -> int * int -> Prop :=
  fun a b => Qint_to_rat b = Some a.
```
- this is a heterogeneous generlization of the abstractions used in _generalized rewriting_
```Coq
R ==> R' : (A -> A') -> (B -> B') -> Prop
```
- we can then define correctness of addition; output holds for the relation too
```Coq
R ==> R' : (A -> A') -> (B -> B') -> Prop
Lemma Rrat_addq : (Rrat ==> Rrat ==> Rrat) +_rat +_int*int.
```
- @refinements then show how the automation corresponds to proof searching the desired relations based on parametricity; $[| ... |] +_"rat" +_Q$, restricted to non dependent types

== Recap Logical Relations

- we have seen logical relations for safety of System F @amin #pause
- the relations are unary; predicates, e.g. $P$ #pause
- safety then is a predicate parameterized by $P$ as follows $ P^upright(bold(E)) (e) = upright(bold("Safe"))_P (e) =^triangle.stroked.small.t ...$
#pause
- the predicates then are defined inductively e.g.
$
  [| Delta hy alpha |]_xi &=^triangle.stroked.small.t xi(alpha) \
  [| Delta hy () |]_xi (v) &=^triangle.stroked.small.t v = tt \
  [| Delta hy tau_1 times tau_2 |]_xi (v) &=^triangle.stroked.small.t exists v_1, v_2. v = (v_1, v_2) and [|
    Delta hy tau_1
  |]_xi (v_1) and [| Delta hy tau_2 |]_xi (v_2) \
  ...
$
#pause
- eventually constructing the fundamental theorem / abstraction theorem
$
  forall xi, v s. [| Delta hy Gamma |]_xi (v s) ==> [| Delta hy tau |]^upright(bold(E))_xi (e[v s slash x s])
$

== Parametricity Translation

*logical relations for equivalence*
- in contrast, now we define a binary logical relation for equivalence / identifications
- for a type $T$, $[|T|]$ is a logical relation of DTT itself rather than first order logic #pause
- given a term $t$ we notate $t'$ as a term where every variable $x$ in $t$ is replaced with a fresh variable $x'$ #pause

*parametricity from abstraction theorem*
$
  Gamma hy t : T ==> &[|Gamma |] hy t : T and \
  &[|Gamma |] hy t' : T' and \
  &[|Gamma |] hy [|t|] : [|T|] t t'
$
#pause
presupposing
$
  hy [| UU_i |] : [| UU_(i+1) |] UU_i UU_i
$

#pagebreak()
*context raw parametricity translation*
$
  [| angle.l angle.r |] &= angle.l angle.r \
  [| Gamma, x:A |] &= [| Gamma |], x : A, x' : A', x_R : [|A|] x x'
$
#pause
*term raw parametricity translation*
$
  [| UU_i |] &= lambda A, A'. A -> A' -> UU_i \
  [| x |] &= x_R
$
#pause
*$Pi$ term raw parametricity translation*
$
  [| A B |] &= [| A |] B B' [| B |] \
  [| lambda x : A, t |] &= lambda x : A, x' : A', x_R : [| A |] x x'. [| t |] \
  [| Pi(x : A, B) |] &= lambda f, f'. Pi(x : A, x' : A', x_R : [|A|]x x', [|B|]f(x)f'(x'))
$

== Univalent Parametricity

#figure(
  diagram(
    cell-size: 10mm,
    $
      A
  #edge("rr", $R$, "<->", shift: 0pt, label-anchor: "south", label-sep: 0em, bend: 30deg)
  #edge("rr", $p : Id(A,a,attach(arrow.b, br: e)(b))$, "<->", shift: 0pt, label-anchor: "north", label-sep: 0em, bend: -30deg)
  #edge((1,-0.25),(1,0.25), $[| UU_i |]$, "<..>")
  & &
B
    $,
  ),
)
$
  [|
    UU_i
  |] A B = Sigma(R : A -> B -> UU_i, e : A equiv B, Pi(a : A, b : B, R a b equiv Id(A, a, attach(arrow.b, br: e)(b))))
$
#align(center)[_ $A$ and $B$ are related on $[| UU_i |]$ if for some $R$ and $e$, $R$ is equivalent to $Id(A,a, attach(arrow.b, br:e)(b))$_]

- univalent parametricity is abstraction theorem for heterogeneous logical relations
- with the above $[| dot |]$ is now called the *univalent parametricity translation*

== Abstraction Theorem

- for any term $T : UU_i$ thats also a type, the *term translation* of $T$ is $[T]$ #pause
  - it is a $Sigma$ term with a relation with additional data prescribed by $[|UU_i|]$
  - $"rel"([T]) = [|T|]$, where $"rel"$ projects the relation #pause
- we notate $hyu$ as a typing judgement assuming univalence

*abstraction theorem for univalent parametricity translation*
$
  Gamma hy t : T ==> [| Gamma |] hyu [t] : [| T |] t t'
$
#pause
presupposes
$
  hyu [ UU_i ] : [| UU_(i+1) |] UU_i UU_i \
  hyu Pi(A : UU_i, B : UU_i, [| UU_i |] A B equiv Id(UU_i, A, B)) \
  "rel"([ UU_i ]) \u{2261} [| UU_i |]
$

= Type Equivalence in Kit

== Redefining Univalence

- lets make the definition of univalence symmetrical in terms of relations
$
  "isContr"(T) &= Sigma(t : T, Pi(t' : T, Id(T,t,t'))) \ #pause
  "isFun"(R) &= Pi(a : A, "isContr"(Sigma(b : B, R(a,b)))) \ #pause
  A equiv B &= Sigma(R : A -> B -> UU, "isFun"(R) times "isFun"(R^(-1))) \
  &script("where" R^(-1) (b,a) = R(a,b))
$
#pause
#align(center)[_we can rewrite $`equiv`$ in terms of $`"isFun"`$; Lemma 3 @trocq _]

$
  A equiv B = underbrace(Sigma(f : A -> B, "isEquiv"(f)), "contractible fibre") = underbrace(Sigma(R, "isFun"(R) times "isFun"(R^(-1))), "functional relation")
$

#pagebreak()
- a univlent map then is defined as follows
$
  "isUmap"(
    R
  ) = Sigma(
    &m : A -> B, && 1 & -> \
    &g_1 : Pi(a : A, b : B, Id(B, m(a), b) -> R(a, b)),  && 2_a & arrow.t \
    &g_2 : Pi(a : A, b : B, R(a, b) -> Id(B, m(a), b)), && 2_b & arrow.b \
    &Pi(a : A, b : B, g_1(a,b) comp g_2(a,b) =^dot_dot id)) && 4 & equiv
$
#align(center)[_we can rewrite $`"isFun"`$ as $`"isUmap"`$; Lemma 4 @trocq _] #pause
- $equiv$ as a type equivalence relation $sqr^top$ in terms of $"isUmap"$ thus is defined
$
  sqr^top (A,B) = Sigma (R : A -> B -> UU_i, "isUmap"(R) times "isUmap"(R^(-1)))
$
$
  A equiv B = underbrace(sqr^top (A,B), "univalent map")
$

== General Equivalence

#slide(
  repeat: 2,
  self => [
    #let (uncover, only, alternatives) = utils.methods(self)
    - $sqr^((n,k))$ generalizes over a product lattice element of $cal(A)^2$ where $cal(A)$ is as follows
    #figure(
      cetz.canvas({
        import cetz.draw: *
        circle((-2, 0), radius: (0.1, 0.1), fill: black, anchor: "mid", name: "0")
        circle((0, 0), radius: (0.1, 0.1), fill: black, anchor: "mid", name: "1")
        circle((2, 1), radius: (0.1, 0.1), fill: black, anchor: "mid", name: "2_a")
        circle((2, -1), radius: (0.1, 0.1), fill: black, anchor: "mid", name: "2_b")
        circle((4, 0), radius: (0.1, 0.1), fill: black, anchor: "mid", name: "3")
        circle((6, 0), radius: (0.1, 0.1), fill: black, anchor: "mid", name: "4")
        content((-2, 1), $0$, anchor: "north")
        content((-2, -0.5), [#text(size: 0.65em, "no data")], anchor: "south")
        content((0, 1), $1$, anchor: "north")
        content((0, -0.5), [#text(size: 0.65em, $->$)], anchor: "south")
        content((2, 2), $2_a$, anchor: "north")
        content((2, 0.5), [#text(size: 0.65em, $arrow.t$)], anchor: "south")
        content((2, -0), $2_b$, anchor: "north")
        content((2, -1.5), [#text(size: 0.65em, $arrow.b$)], anchor: "south")
        content((4, 1), $3$, anchor: "north")
        content((4, -0.5), [#text(size: 0.65em, $iso$)], anchor: "south")
        content((6, 1), $4$, anchor: "north")
        content((6, -0.5), [#text(size: 0.65em, $equiv$)], anchor: "south")
        line("0.east", "1.west", name: "line0", mark: (end: ">", start: none))
        line("1.east", "2_a.west", name: "line1", mark: (end: ">", start: none))
        line("1.east", "2_b.west", name: "line2", mark: (end: ">", start: none))
        line("2_a.east", "3.west", name: "line3", mark: (end: ">", start: none))
        line("2_b.east", "3.west", name: "line4", mark: (end: ">", start: none))
        line("3.east", "4.west", name: "line5", mark: (end: ">", start: none))
      }),
    )
    $
      sqr^((n,k)) (A, B) &= Sigma(R : A -> B -> UU, M_n(R) times M_k(R^(-1)))
    $
    #alternatives[
      $
        M_0(R) &= tt \
        M_1(R) &= A -> B
      $
    ][
      $
        M_(2_a)(
          R
        ) &= Sigma(m : A -> B, G_(2_a)(m,R)) script("where" G_(2_a)(m,R) = Pi(a : A, b: B, Id(B, m(a), b) -> R(a,b))) \
        M_(2_b)(
          R
        ) &= Sigma(m : A -> B, G_(2_b)(m,R)) script("where" G_(2_b)(m,R) = Pi(a : A, b: B, R(a,b) -> Id(B, m(a), b))) \
        M_3(R) &= Sigma(m : A -> B, (G_(2_a)(m,R) times G_(2_b)(m,R))) \
        M_4(R) &= Sigma(m : A -> B, Sigma(g_1 : G_(2_a)(m,R), g_2 : G_(2_b)(m,R), g_1(a,b) comp g_2(a,b) =^dot_dot id))
      $
    ]
  ],
)

#pagebreak()

- we have a general type equality relation on $A$ and $B$ as follows
$
  sqr^((m,n)) (A,B) &= sqr^((n,m)) (B,A) \ #pause
sqr^top = sqr^((4,4)) &= A equiv B \ #pause
sqr^((3,3)) &= A iso B \ #pause
sqr^((1,0)) &= A -> B \ #pause
$
moreover
- $sqr^((4,0))$ is a univalent map in just the direction from $A$ to $B$
- $sqr^((4,2_a))$ is a surjective univalent map with a partial left inverse
- $sqr^((4,2_b))$ is a injective univalent map with partial right inverse

#pagebreak()

- revisiting univalent parmetricity translation, we can restate the abstraction theorem's presuppositions as follows
$
  hyu p_square^(alpha,beta) : sqr^beta UU UU \
  "rel"(p_square^(alpha,beta)) = sqr^alpha
$
#pause
- note how $p_square^(top,top)$ corresponds to $[UU]$ i.e. $"rel"([UU]) = sqr^top$ #pause
- however this does not hold for any arbitrary pairs of $alpha,beta$, it only holds for:
$
  cal(D)_square = {(alpha,beta) in cal(A)^2 | alpha = top or beta in {0,1,2_a}^2}
$
- $beta$ depends on univalence / $alpha=(4,4)$ if $beta in {0,1,2_a}^2$; insufficient "data" otherwise

#pagebreak()

- we then do the same for our type formers e.g. $Pi$
#figure(
  proof-tree(
    rule(
      $Gamma hy p_Pi^gamma (A_R,B_R) : sqr^gamma (Pi(x : A, B), Pi(x' : A', B'))$,
      $Gamma hy A_R : sqr^alpha (A, A')$,
      $Gamma, x : A, x' : A', x_R : A_R (x, x') hy B_R : sqr^beta (B, B')$,
    ),
  ),
) #pause
- $gamma$ determines the minimum required "data" from $alpha, beta$
$
  D_Pi (gamma) &= (alpha, beta) \
  D_Pi (m,n) &= ((m_A, n_A), (m_B, n_B)) \
$ #pause
- $p_Pi^((m,n))$ is computed from $p_Pi^((m,0))$ and $p_Pi^((0,n))$ but by symmetry we can compute $p_Pi^((n,0))$
$
  D_Pi (m,0) &= ((0, n_A), (m_B, 0)) \
  D_Pi (n,0) &= ((0, m_A), (n_B, 0))
$
- thus we only need to compute $p_Pi^((m,0))$

#pagebreak()
- the `Trocq` meta program computed the following for $p_Pi^((m,0))$ and $p_->^((m,0))$
#figure(
  table(
    columns: 9,
    align: (center, center, center, center, center, center, center, center, center),
    table.cell(rowspan: 2)[$m$],
    table.cell(colspan: 4)[$D_Pi (m,0)$],
    table.cell(colspan: 4)[$D_-> (m,0)$],
    $m_A$, $n_A$, $m_B$, $n_B$, $m_A$, $n_A$, $m_B$, $n_B$,
    $0$, $0$, $0$, $0$, $0$, $0$, $0$, $0$, $0$,
    $1$, $0$, $2_a$, $1$, $0$, $0$, table.cell(fill: orange.lighten(80%), $1$), $1$, $0$,
    $2_a$, $0$, $4$, $2_a$, $0$, $0$, table.cell(fill: orange.lighten(80%), $2_b$), $2_a$, $0$,
    $2_b$, $0$, $2_a$, $2_b$, $0$, $0$, $2_a$, $2_b$, $0$,
    $3$, $0$, $4$, $3$, $0$, $0$, table.cell(fill: orange.lighten(80%), $3$), $3$, $0$,
    $4$, $0$, $4$, $4$, $0$, $0$, $4$, $4$, $0$
  ),
)
- note how the non dependent functions dont require as much information from $p_->^((m,0))$
- we can imagine the same being done for other type formers
- thus we can perform proof transfer without needing $(4,4)$; full univalence, in all cases
= `Trocq` Calculus

== Raw Parametricity Sequents

- *parametricity contexts* are duplicate free list of terms with a witness of their relation
$
  Xi ::= epsilon | Xi, x ~ x' ∵ x_R
$
- we notate that if $x$ is in the list with $Xi(x) = (x', x_R)$ #pause
- *parametricity judgement* then come in the form $Xi hy M ~ M' ∵ M_R$ #pause
#figure(
  proof-tree(
    rule(
      name: [defn-eq-param],
      $Xi hy (M',M_R) = (N', N_R)$,
      $Xi hy M ~ M' ∵ M_R$,
      $Xi hy M ~ N' ∵ N_R$,
    ),
  ),
)
- this way our calculus *internalizes* the parametricity translation $[| dot |]$ from before

#pagebreak()

#figure(
  proof-tree(
    rule(
      name: [param-sort],
      $Xi hy UU_i ~ UU_i ∵ lambda A, B : UU_i . A -> B -> UU_i$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [param-var],
      $Xi hy x ~ x' ∵ x_R$,
      $Xi(x) = (x',x_R)$,
      $Xi hy$,
    ),
  ),
)
#pause
corresponds to raw parametricity term translations
$
  [| UU_i |] &= lambda A, A'. A -> A' -> UU_i \
  [| x |] &= x_R
$
#pause
- likewise for $Pi$
#figure(
  proof-tree(
    rule(
      $Xi hy Pi(x : A, B) ~ Pi(x' : A', B') ∵ lambda f, g. Pi(x : A, x' : A', x_R : [|A|](x,x'), B_R (f(x),g(x')))$,
      $Xi hy A ~ A' ∵ A_R$,
      $Xi, x ~ x' ∵ x_R hy B ~ B' ∵ B_R$,
    ),
  ),
)


#pagebreak()

- $Xi$ provides parametricity whilst $Gamma$ provides typing for the terms; $Gamma(x) = A <=> Gamma hy x : A$
$
  Gamma trir Xi <=> ((Xi hy Gamma(x) ~ A' ∵ A_R) => (Gamma(x') = A' and Gamma(x_R) = A_R(x,x')))
$ #pause
- we can then internalize the abstraction theorem as a rule
#figure(
  proof-tree(
    rule(
      name: [abstr-thm],
      $Gamma hy M' : A' #h(1em) Gamma hy M_R : A_R (M,M')$,
      $Gamma trir Xi$,
      $Gamma hy M : A$,
      $Xi hy M ~ M' ∵ M_R$,
      $Xi hy A ~ A' ∵ A_R$,
    ),
  ),
)
corresponds to the abstraction theorem for raw parametricity translation
$
  Gamma hy t : T ==> &[|Gamma |] hy t : T and \
  &[|Gamma |] hy t' : T' and \
  &[|Gamma |] hy [|t|] : [|T|] t t'
$

== Univalent Parametricity Sequents

- we now augment the parametricity terms with $sqr^top$

#figure(
  proof-tree(
    rule(
      name: [UParam-sort],
      $Xi hyu UU_i ~ UU_i ∵ p_(square_i)^(top,top)$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [UParam-var],
      $Xi hyu x ~ x' ∵ x_R$,
      $Xi(x) = (x',x_R)$,
      $Xi hy$,
    ),
  ),
)
#pause
#figure(
  proof-tree(
    rule(
      name: [UParam-Pi],
      $Xi hyu Pi(x : A, B) ~ Pi(x' : A', B') ∵ p_Pi^top ( A_R, B_R)$,
      $Xi hyu A ~ A' ∵ A_R$,
      $Xi, x ~ x' ∵ x_R hyu B ~ B' ∵ B_R$,
    ),
  ),
)

#pagebreak()

the abstraction theorem thus becomes
#figure(
  proof-tree(
    rule(
      name: [Uabstr-thm],
      $Gamma hy M' : A' #h(1em) Xi hyu M_R : "rel"(A_R)(M,M')$,
      $Gamma trir Xi$,
      $Gamma hy M : A$,
      $Xi hyu M ~ M' ∵ M_R$,
      $Xi hyu A ~ A' ∵ A_R$,
    ),
  ),
)
- note that the terms are related by $M_R$ which is a witness of type relations $A_R : "rel"(p_(UU_i)^(top,top)) (A, A')$ which is a witness of univalence $"rel"(p_(UU_i)^(top,top)) = sqr^top (A, A')$

== CoC w annotations ($"CC"_omega^+$)

- now we weaken our univalent parametricity translation for sorts with weaker structures as in our lattice elements $cal(A)$
- universes are now annotated with $alpha in cal(A)$ as follows $UU_i^alpha$ to govern the amount of information available in parametricity witnesses #pause
- with annotations we have a new subtyping judgement
#figure(
  proof-tree(
    rule(
      name: [SubSort],
      $Gamma hyp UU_i^alpha subt UU_j^beta$,
      $alpha >= beta$,
      $i <= j$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [SubConv],
      $Gamma hyp A subt B$,
      $Gamma hyp A : K$,
      $Gamma hyp B : K$,
      $A #math.equiv B$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [SubPi],
      $Gamma hyp Pi(x : A, B) subt Pi(x : A', B')$,
      $Gamma hyp Pi(x : A, B : UU_i)$,
      $Gamma hyp A' subt A$,
      $Gamma, x : A' hyp B subt B'$,
    ),
  ),
)
- where $K := UU_i | Pi(x : A, K)$

#pagebreak()

#figure(
  proof-tree(
    rule(
      name: [Sort+],
      $Gamma hyp UU_i^alpha : UU_(i+1)^beta$,
      $(alpha, beta) in cal(D)_square$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [Conv+],
      $Gamma hyp M : B$,
      $Gamma hyp M : A$,
      $Gamma hyp A subt B$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [Pi+],
      $Gamma hyp Pi(x : A, B : UU_i^gamma)$,
      $Gamma hyp A : UU_i^alpha$,
      $Gamma, x : A hyp B : UU_i^beta$,
      $D_Pi (gamma) = (alpha, beta)$,
    ),
  ),
)
- we have annotated our types with $cal(A)^2$

== `Trocq` Calculus


- `Trocq` calculus builds the general univalent parametricity translations internally on $"CC"_omega^+$
- its objective is for proof transfer rather than to check if terms are in relation
- now we add type information needed for the automatic translation on relation structures
$
  Delta ::= epsilon | Delta, x @ A ~ x' ∵ x_R
$
- thus judgements are 4-ary relation $(M,A,M',M_R)$, term, type, term, param witness
$
  Delta hyt M @ A ~ M' ∵ M_R
$

#pagebreak()
- we have a weaken operation that forgets fields from $M_p$ to $M_q$ where $p >= q$ in $cal(A)$
$
  attach(arrows.bb, br: (p,q), tr: (m,n)) angle.l R, M^->, M^<- angle.r := angle.l R, attach(arrow.b, tr: m, br: p) M^->, attach(arrow.b, tr: n, br: q) M^<- angle.r
$ #pause
- weaken on parametricity witnesses $attach(arrow.b.double, tr: T, br: U)$ is defined inductively
$
  attach(arrow.b.double, tr: UU_i^alpha, br: UU_i^alpha') t_R &= attach(arrows.bb, tr: alpha, br: alpha') t_R \
  attach(arrow.b.double, tr: A, br: A') M_R &= M_R \
  attach(arrow.b.double, tr: Pi(x : A, B), br: Pi(x : A', B')) M_R &= lambda x, x', x_R. attach(arrows.bb, tr: B, br: B') (M_R (x,x',attach(arrow.b.double, tr: A', br: A) x_R))
$

#pagebreak()

#figure(
  proof-tree(
    rule(
      name: [TrocqSort],
      $Delta hyt UU_i^alpha @ UU_(i+1)^beta ~ UU_i^alpha ∵ p_(UU_i)^(alpha,beta)$,
      $(alpha, beta) in cal(D)_square$,
    ),
  ),
) #pause
#figure(
  proof-tree(
    rule(
      name: [TrocqVar],
      $Delta hyt x @ A ~ x' ∵ x_R$,
      $(x, A, x', x_R) in Delta(x)$,
      $gamma(Delta) hyp$,
    ),
  ),
) #pause
#figure(
  proof-tree(
    rule(
      name: [TrocqPi],
      $Delta hyt Pi(x : A, B) @ UU_i^delta ~ Pi(x' : A', B') ∵ p_Pi^delta (A_R,B_R)$,
      $(alpha, beta) = cal(D)_Pi (delta)$,
      $Delta hyt A @ UU_i^alpha ~ A' ∵ A_R$,
      $Delta, x @ A ~ x' ∵ x_R hyt B @ UU_i^beta ~ B' ∵ B_R$,
    ),
  ),
) #pause
#figure(
  proof-tree(
    rule(
      name: [TrocqConv],
      $Delta hyt M @ B ~ M' ∵ attach(arrow.b.double, br: B, tr: A) M_R$,
      $Delta hyt M @ A ~ M' ∵ M_R$,
      $gamma(Delta) hyp A subt B$,
    ),
  ),
)

#pagebreak()

- thus the abstraction theorem for `Trocq`:
#figure(
  proof-tree(
    rule(
      name: [TrocqAbs-Thm],
      $gamma(Delta) hyp M' : A' #h(2em) gamma(Delta) hyp M_R : "rel"(A_R)(M, M')$,
      $gamma(Delta) hyp$,
      $gamma(Delta) hyp M : A$,
      $Delta hyt M @ A ~ M' ∵ M_R$,
      $Delta hyt A @ UU_i^alpha ~ A' ∵ A_R$,
    ),
  ),
)

== Constants

- constants $c in cal(C)$ are primitives u have in the calculus stored in a global context
- crucially it can be assigned several different annotations $T_c subset cal(T)_("CC"_omega^+)$; i.e. polymorphic on $alpha$
- thus translation for each annotation possible is provided; $cal(D)_c$

#figure(
  proof-tree(
    rule(
      name: [Const+],
      $Gamma hy c : A$,
      $c in cal(C)$,
      $A in T_c$,
    ),
  ),
)
#figure(
  proof-tree(
    rule(
      name: [TrocqConst],
      $Delta hy c @ A ~ c' ∵ c_R$,
      $D_c (A) = (c', c_R)$,
    ),
  ),
)

= Conclusion

== Summary

- we use logical relations to relate terms under equality; $[| A |] t t'$
- relations on type equality is generalized with $sqr^alpha$
- *Raw Parametricity Sequents* internalize parametricity with $x ~ x' ∵ x_R$ judgements
- *Univalent Parametricity Sequents* internalizes the generalized equality relation $sqr^alpha$
- *$"CC"_omega^+$* annotates types with $alpha$ along with a subtyping operator $subt$ for the annotations
- *`Trocq`* adds type information to the internalized $sqr^alpha$ judgements necessary to automate proof transfer along with weaken operations $arrows.bb, arrow.b.double$; _think down casting to appropriate $cal(A)$_

== Example

- recall in our motivation example for proof transfer on types $[0,+infinity)$ and $[0,+infinity]$ #pause
- $u : NN -> [0, +infinity)$ is a sequence of non negative real numbers #pause
- $u$ is summable when $Sigma u = (Sigma_(k=0)^n u_k)_(n in NN)$ has a finite limit #pause
- algebraic operations (e.g. commutativity) require a proof of summability for each step #pause
- the standard approach assigns a default value to the case of an infinite sum thus extending the domain to $[0,+infinity]$ #pause
- with this domain a limit is always defined #pause
- we prove in the extended domain and proof transfer to the original #pause
- lets automate this with `Trocq` for addition

#pagebreak()
#figure(grid(
  columns: 3,
  align: (center + horizon, center + horizon),
figure(
  diagram(
    cell-size: 5mm,
    $
      dash(RR)_(>=0)
    #edge("r", [`truncate`$\ ^(2_b)$], bend: 30deg, "->") &
  RR_(>=0)
    #edge("l", [`Fin`$\ ^4$], bend: 30deg, "->")
    $,
  ),
),
h(3em),
figure(
  diagram(
    cell-size: 5mm,
    $
      #[`summable`]
    #edge("r", [`to_seq`$\ ^(2_b)$], bend: 30deg, "->") &
  #[`seq`$\ _(dash(RR)_(>=0))$]
    #edge("l", [`seq_extend`$\ ^4$], bend: 30deg, "->")
    $,
  ),
)
))

$
  #[`Definition `] Sigma_(RR_(>= 0)) (u : #[`summable`]) : RR_(>=0) := #[`truncate`] (Sigma_(dash(RR)_(>=0)) (#[`seq_extend`] (u)))
$
#figure(
  diagram(
    cell-size: 5mm,
    $
      Sigma_(dash(RR)_(>=0)) (u + v) = Sigma_(dash(RR)_(>=0)) (u) + Sigma_(dash(RR)_(>=0)) (v)
    #edge("r", [`trocq`], "->") &
  Sigma_(RR_(>=0)) (u + v) = Sigma_(RR_(>=0)) (u) + Sigma_(RR_(>=0)) (v)
    $,
  ),
)

= Bibliography <touying:hidden>

#bibliography("refs.bib")

= Thank You <touying:hidden>

#focus-slide[
  #smallcaps([Thank You])
]