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
    subtitle: [HoTT to Parametric Univalence and Beyond; `Trocq`],
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
- *`Trocq` calculus*: generalizes proof transfer to be done between proofs beyond type equivalence, e.g. the type $[0,+ infinity)$ extended to $[0, +infinity]$ where the transfer for $+infinity$ have to be done manually, but with `Trocq` is automated

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
- how do we construct a function $"isEven"_N$ for big unsigned integers without having to introspect the "structure" of $N$?

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
- we want $attach(arrow.t, br: B)$ to brings proofs in $A$ to $B$, and  $attach(arrow.b, br: B)$ vice versa #pause
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
#pause

== Example $N, bb(N)$


*different types*; _type equivalence_
#figure(grid(columns: 2, align: (center + horizon, center + horizon),
$
  A =& bb(N) times (bb(N) -> bb(N)) \
  V =& Pi(X : bb(N), Pi(P : bb(N) -> UU, \
  &P(X.1) -> \
  &Pi(n : bb(N), P(n) -> P(X.2(n)) -> \
  &Pi(n : bb(N), P(n)))))
$, pause,
$
  B =& N times (N -> N) \
  W =& Pi(X : N, Pi(P : N -> UU, \
  &P(X.1) -> \
  &Pi(n : N, P(n) -> P(X.2(n)) -> \
  &Pi(n : N, P(n)))))
$
))
#align(center)[_$R_T$ is a type equivalence between $bb(N)$ and $N$ transporting $V(a)$ to $W(a)$ along this equivalence_]

#pagebreak()
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
- they differ by the relations they can express #pause
- *generalized rewriting* provide support to setoid based formulations for homogeneous functional relations; $A=B$ #pause
- *`CoqEAL`* library provide support to refinements specialized to heterogeneous functional relations; $A != B$, in quantifier free type formers #pause
- we now discuss transfer methods for dependent types and heterogeneous relations in general :)

== Parametricity Translation

*notation*
- given a type $T$, we notate $[|T|]$ as a logical relation #pause
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
      a
  #edge("rr", $R$, "<->", shift: 0pt, label-anchor: "south", label-sep: 0em, bend: 30deg)
  #edge("rr", $p : Id(A,a,attach(arrow.b, br: e)(b))$, "<->", shift: 0pt, label-anchor: "north", label-sep: 0em, bend: -30deg)
  #edge((1,-0.25),(1,0.25), $[| UU_i |]$, "<-->")
  & &
b
    $,
  ),
)
$
  [| UU_i |] A B = Sigma(R : A -> B -> UU_i, e : A equiv B, Pi(a : A, b : B, R a b equiv Id(A, a, attach(arrow.b, br: e)(b))))
$
#align(center)[_ $A$ and $B$ are related on $[| UU_i |]$ if for some $R$ and $e$, $R$ is equivalent to $Id(A,a, attach(arrow.b, br:e)(b))$_]
#pause
#figure(
  diagram(
    cell-size: 10mm,
    $
      A
  #edge("rr", $attach(arrow.t arrow.b, br: e)$, "<-->", shift: 0pt, label-anchor: "south", label-sep: 0em, bend: 30deg)
  #edge("rr", $[| UU_i |]$, "<-->", shift: 0pt, label-anchor: "north", label-sep: 0em, bend: -30deg)
  #edge((1,-0.25),(1,0.25), $u$, "<-->")
  & &
B
    $,
  ),
)
$
  [| UU_i |] A B equiv (A equiv B)
$

#pagebreak()

- univalent parametricity is abstraction theorem for heterogeneous logical relations
- with the above $[| dot |]$ is now called the univalent parametricity translation

== Term Translation

- for any term $T : UU_i$ thats also a type, the translation of $T$ as a term is $[T]$ #pause
- it is a $Sigma$ term with a relation with additional data prescribed by $[|UU_i|]$
- in other words: $"rel"([T]) = [|T|]$, where $"rel"$ projects the relation #pause
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

== 

- 3.1 ap, isFun, isUmap, symmetrical univalence
- 3.2 more on the unimap relation
- 3.3 hierarchical unimap relation

= `Trocq` Calculus

== 

- 4.1 raw parametric sequents, abstraction theorem
- 4.2 univalent parametricity sequents
- 4.3 annotated tt
- 4.4 Trocq calculus

= Conclusion

== Example

- section 5

= Bibliography <touying:hidden>

#bibliography("refs.bib")

= Thank You <touying:hidden>

#focus-slide[
  #smallcaps([Thank You])
]