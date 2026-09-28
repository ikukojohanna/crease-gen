# folding

A crease pattern generator in Scala 3. It takes folding rules, produces the
geometric blueprint, folds it, and stacks the layers.

The claim this code is built to support: **origami is a form of computation, and
the mathematics that decides which folds are physically valid is a type system.**
Everything here follows from taking that literally.

```
scala-cli run .          # every model -> out/*.svg, out/*-folded.svg, out/*.fold
scala-cli run . -- out miura-ori bird-base
scala-cli test .
```

Each model that folds produces three files: the crease pattern to print, the
folded model to look at, and a [FOLD file](https://github.com/edemaine/fold) to
drop into [Origami Simulator](https://origamisimulator.org). Same data, three
materials: types, pixels, paper.

## The pipeline

```
CreasePattern --planarize--> PlanarGraph --Laws--> FlatFoldable
                                  |                     |
                               Faces                FoldedState      (fold every facet)
                                                        |
                                                     Layers          (stack them)
                                                        |
                                                   FoldedModel
```

| Claim | File |
|---|---|
| A fold is a reflection; that is the whole instruction set | `geometry/` (`Line`, `Axioms`, `Rigid`) |
| Immutability is not a style choice, it is the physics | `pattern/CreasePattern.scala` |
| A drawing is not a structure until you planarise it | `pattern/Planarize.scala`, `pattern/Faces.scala` |
| Mathematical laws as type constraints | `laws/Laws.scala`, `laws/FlatFoldable.scala` |
| Constraints leave freedom, so labelling is search | `laws/Assigner.scala` |
| A folded model is a product of reflections | `folding/FoldedState.scala` |
| Where the local laws run out | `folding/Layers.scala`, `folding/Overlap.scala` |
| The proof-carrying result | `folding/FoldedModel.scala` |
| The blueprint | `output/Svg.scala`, `output/Fold.scala` |
| Worked examples, including ones that must fail | `library/Patterns.scala` |

## The physics forces the functional style

**Points and vectors are different types.** A location on the sheet and a
displacement across it are not the same thing. `point + vector` is a point,
`point - point` is a vector, and `point + point` does not compile, because it
does not mean anything.

**Equality is physical.** Two points are the same point when you could not tell
them apart by creasing. `Tol` is an explicit `using` parameter, so no comparison
in this program can quietly pretend to be exact.

**Every operation returns a new pattern.** Not for elegance: a crease is a
permanent change to the material. You cannot un-crease paper, so `CreasePattern`
has no mutating operation to offer. A crease pattern is the entire history of a
sheet, and history does not mutate.

**One invariant, held by construction.** Every crease lies on the paper. The
constructor is private and the only way in is a blank sheet; adding a fold clips
it to the sheet, and a fold that misses adds nothing. You cannot fold what is not
there, so the type cannot hold what is not there.

## Two types, two different promises

`FlatFoldable` and `FoldedModel` are both opaque, both have no public
constructor, and the difference between them is the whole second half of the
story.

**`FlatFoldable` means every vertex is happy.** Three local theorems, checked
once at the boundary and never rechecked downstream:

- **Kawasaki-Justin.** Around any interior vertex the sectors alternately add and
  subtract to nothing. A property of the drawing alone.
- **Maekawa-Justin.** Mountains minus valleys is exactly plus or minus two.
  Corollary: an odd number of creases can never meet at a flat-folded vertex.
- **Big-little-big.** A sector strictly smaller than both neighbours is squeezed
  between its creases; if they folded the same way the paper would pass through
  itself.

**`FoldedModel` means the paper is happy.** Every facet has a position and the
layers have an order. And the two are not the same claim -- which is the point.

## Kawasaki's theorem is a well-definedness proof

`FoldedState` is built by walking the facets and composing reflections: start
anywhere, call that facet's position the identity, and each time you cross a
folded crease, compose with the reflection in that crease line. A folded model
is a product of reflections indexed by which facet you are looking at. That is
the whole algorithm, and it is the whole of origami.

The walk raises the obvious question -- the facets form loops, so does the answer
depend on the route? It does not, and the reason is Kawasaki. Reflections in
lines through a common point compose to a rotation by twice the alternating sum
of the angles between them, so the loop around an interior vertex is the identity
exactly when that alternating sum vanishes.

Kawasaki's theorem is not a rule we check on the side. **It is the statement that
this function is well defined.** `FoldingSuite` asserts exactly that: break
Kawasaki and `FoldedState.from` returns `Left`.

## And Maekawa is a stacking constraint in disguise

`Layers` never counts mountains and valleys. What it knows is that the order at a
fold is not a choice: valley-fold a face-up sheet and the moving facet lands on
top, mountain-fold it and it goes underneath, and turning the paper over swaps
the two. So every folded crease pins one relation before the search begins.

Put four mountains at the centre of a square and those forced relations close
into a cycle that no order can satisfy. Maekawa's theorem falls out of the layer
constraints without being stated -- there is a test for that too.

## Where the local laws run out

Justin's conditions say when a stacking is physically possible. Each is the same
sentence in a different costume: *paper does not pass through paper.*

- **taco-taco.** Two folds landing on the same line. Each is a taco of two
  facets, closed at the crease. One may nest entirely inside the other, or miss
  it entirely, but they may not interleave.
- **taco-tortilla.** A fold whose crease lands inside a flat facet. That facet
  cannot slip between the two halves of the taco.
- **tortilla-tortilla.** Two facets joined along an unfolded crease are one
  continuous sheet, so nothing is above one and below the other.

Taco-taco is the one that is easy to get wrong, and this code got it wrong first.
"The two tacos must not interleave" is *not* "neither facet of one lies between
the facets of the other" -- nesting is legal, and nesting is what happens every
time you fold several layers at once. The correct statement is a parity condition
on four facets, and it is the only constraint here that is not a simple equality.

Each condition has a switch (`Layers.Conditions`), so you can turn one off and
watch the checker start accepting the impossible. There is a test that does.

## A pleat and a roll are the same to the laws

Parallel creases that all fold the same way make a **roll**: the sheet spirals
shut around a point. Parallel creases that alternate make a **pleat**: the sheet
opens and closes. The difference is the entire behaviour of a Miura-ori.

Every law in this program is indifferent between them. Both satisfy Maekawa at
every vertex. Both satisfy Kawasaki, because the geometry is identical. Both have
a valid layer ordering. And both fold flat to *exactly the same outline*, because
the folded position of a facet is a product of reflections and reflections do not
know mountain from valley. Only the layer order differs -- and the motion.

This program shipped a rolled Miura-ori for a while, and every check passed. It
was caught in a simulator, by watching it fold. Two things came out of that:

- `Patterns.miura` now alternates in **both** directions. Along a column
  Maekawa forces it; across a band nothing forces it, and it has to be written
  down. There is a test.
- `Assigner` knows which creases are parallel neighbours and tries the
  alternating branch first (`parallelNeighbours`). It is a *hint* -- it only
  orders the two branches, rules nothing out, and cannot make an invalid
  labelling valid. But without it, a search that tries Mountain first returns
  rolls, and the Yoshimura went from needing 51 labellings to needing 1.

The moral is not that the checker was wrong. The checker was right: those were
valid flat foldings. It is that **validity was never the property I wanted**, and
no amount of checking would have told me so. That took paper.

## Four things the model discovered that the diagram hides

**Only three of the four obvious lines on a square can be folded.** Two diagonals
and two midlines meet at the centre in eight creases, and the symmetric
four-four labelling everybody draws is not foldable. So `waterbombBase` folds
three lines and leaves the fourth creased but flat -- and folds to a triangle
exactly 200 x 100 with exactly 4 layers, which is the textbook answer. The
theorem found that, not the picture.

**The classic hyperbolic paraboloid is rejected, at every single vertex.** Two
ring edges of one kind and two diagonal halves of another: two and two, where
Maekawa needs three and one. This is correct. The hypar exists in paper because
the paper *bends*; Demaine, Demaine, Hart, Price and Tachi proved in 2011 that it
is not a folding of flat facets at all. A checker that passed it would be lying.

**A local rule can be right everywhere and wrong overall.** `uniformRule` labels
a waterbomb tessellation with one rule: grid lines mountain, diagonals valley.
Every interior vertex satisfies Kawasaki, Maekawa and big-little-big -- four
mountains to two valleys, at both kinds of vertex, at every size of sheet. It
folds at two cells square. It folds at three. **It fails at four and never works
again.** There is no vertex to point at and no sheet small enough to test on.
Local verification is not verification.

**Passing the local laws leaves an enormous amount of freedom, and most of it is
lies.** The bird base geometry admits well over a hundred thousand labellings
that satisfy every local law. The program searches them and the 225th is the
first that actually folds. Kawasaki tells you a figure is foldable; it does not
tell you which way.

## The axioms

`Axioms.scala` implements all seven Huzita-Hatori axioms: the complete
instruction set of a sheet of paper. Each is a pure, total function from
references already on the sheet to the folds those references determine, and the
return type carries the mathematics. O1 and O2 determine at most one fold, so
they return `Option`. O3 and O5 may determine two and O6 up to three, so they
return `List`. The arity of the answer is part of the signature.

O6 is the one that matters. It places two points onto two lines at once -- a
common tangent to two parabolas -- and solving it means solving a cubic. That
single fold is why paper trisects angles and doubles cubes, and why origami is
strictly stronger than straightedge and compass.

`birdBase` is built this way. The eight slanted creases are not drawn at 22.5
degrees; they are *found*, by folding each edge of the square onto the diagonal
beside it (O3), and the kite points where they land are intersections rather than
measurements. Nothing in that code knows about the square root of two. The paper
works it out -- and the folded model comes out with a 22.5 degree tip and every
raw edge of the square lying on one line, which is what a petal fold does.

## The one search that is allowed to give up

Deciding whether a valid labelling can be stacked without self-intersection is
NP-hard (Bern and Hayes, 1996). So `Layers` and `FoldedModel.search` carry
budgets and can return `GaveUp` or `NoLabelling`, and the program says so rather
than pretending. Everything else here is a check; this is a search, and the
difference is not an implementation detail. It is a complexity-theoretic wall
inside a sheet of paper.

## Status

Working end to end; 7 of the 11 library models fold, and the 4 that do not are
the ones that should not. Known gaps:

- **Nothing here can tell a good folding from a valid one.** The search returns
  the first labelling that folds, and for the tessellations that is an arbitrary
  member of a very large family. The parallel-neighbour hint biases it towards
  pleats, which is a heuristic standing in for a design intention the program has
  no way to represent.
- **The layer search is naive.** Depth-first with transitivity propagation. A
  real implementation would hand the constraints to a SAT solver, and the
  constraints are already in the right shape for it -- parity clauses over
  pairwise order variables.
- **No 3D, and no rigid folding.** Only the flat folded state. The Miura's
  one-degree-of-freedom motion is the obvious next thing, and it needs dihedral
  angles rather than reflections.
- **Exact arithmetic.** Doubles plus an explicit tolerance work, but
  constructible coordinates would let the equality be real equality.
- **More models:** square twist, Resch, Kawasaki rose, Lang tree-method bases.
- The folded silhouettes are computed, not checked against reference models.
  The two classic bases come out exactly right (preliminary base 100x100 at 4
  layers, waterbomb base 200x100 at 4 layers), which is the main evidence the
  machinery is correct.
