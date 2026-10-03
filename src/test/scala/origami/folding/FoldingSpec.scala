package origami.folding

import origami.geometry.{Line, Pt, Rigid, Tol}
import origami.laws.{Assigner, Laws, Violation}
import origami.library.{Model, Patterns}
import origami.pattern.{Assignment, CreasePattern, Faces, PlanarGraph}

class FoldingSpec extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)

  private def fold(m: Model): FoldedModel =
    val g = m.pattern.planarize
    val r =
      if g.edges.exists(_.assignment == Assignment.Optional) then
        FoldedModel.searchAll(g, m.symmetries, rankMillis = 0).map(_.best)
      else FoldedModel.of(g)
    r.fold(why => fail(s"${m.name} did not fold: ${why.explain}"), identity)

  private def dims(f: FoldedModel): (Double, Double) =
    val b = f.state.bounds
    (b.width, b.height)

  test("a facet describes its corners the same way twice") {
    Vector(Patterns.birdBase(), Patterns.deadEnd(), Patterns.yoshimura()).foreach: m =>
      val g = m.pattern.planarize
      Faces(g).facets.foreach: f =>
        assertEquals(f.vertices.map(g.vertices), f.polygon.vertices, m.name)
  }

  test("a rigid motion is a rigid motion") {
    val r = Rigid.reflection(Line.through(Pt(1, 2), Pt(5, -1)))
    val (p, q) = (Pt(3, 9), Pt(-4, 2))
    assertEqualsDouble(r(p).distTo(r(q)), p.distTo(q), 1e-9)
    assertEquals(r.det < 0, true, "a reflection reverses orientation")
    assert(r.compose(r).approx(Rigid.identity), "reflecting twice is the identity")
  }

  test("facets tile the sheet, slits and all") {
    val models = Vector(Patterns.birdBase(), Patterns.miura(), Patterns.waterbombBase(),
      Patterns.deadEnd(), Patterns.uniformRule)
    models.foreach: m =>
      val faces = Faces(m.pattern.planarize)
      val total = faces.facets.map(_.polygon.area).sum
      assertEqualsDouble(total, m.pattern.paper.area, 1e-6, m.name)
  }

  test("Euler holds for the crease graph") {
    Vector(Patterns.birdBase(), Patterns.miura(), Patterns.yoshimura()).foreach: m =>
      val g = m.pattern.planarize
      val f = Faces(g).facets.length + 1  // plus the outer face
      assertEquals(g.vertices.length - g.edges.length + f, 2, m.name)
  }

  test("the accordion folds to one panel, as many layers as panels") {
    val f = fold(Patterns.accordion(8, 200))
    val (w, h) = dims(f)
    assertEqualsDouble(w, 25.0, 1e-6)
    assertEqualsDouble(h, 200.0, 1e-6)
    assertEquals(f.thickness, 8)
  }

  test("the waterbomb base is a triangle four layers thick") {
    val f = fold(Patterns.waterbombBase(200))
    assertEqualsDouble(dims(f)._1, 200.0, 1e-6)
    assertEqualsDouble(dims(f)._2, 100.0, 1e-6)
    assertEquals(f.thickness, 4)
  }

  test("the preliminary base is a square of half the side, four layers thick") {
    val f = fold(Patterns.preliminaryBase(200))
    assertEqualsDouble(dims(f)._1, 100.0, 1e-6)
    assertEqualsDouble(dims(f)._2, 100.0, 1e-6)
    assertEquals(f.thickness, 4)
  }

  test("in the bird base every raw edge lands on the same line") {
    val f = fold(Patterns.birdBase(200))
    val rim = f.graph.edges.indices
      .filter(e => f.graph.edges(e).assignment == Assignment.Boundary)
      .flatMap(e => f.state.foldedEdge(e))
    assert(rim.length >= 8)
    val l = rim.head.line
    rim.foreach(s => assert(l.contains(s.a) && l.contains(s.b),
      "a petal fold brings every raw edge to the centre line"))
  }

  test("folding preserves area, and the layers account for all of it") {
    val f = fold(Patterns.miura())
    val total = f.foldedFacets.map(_.area).sum
    assertEqualsDouble(total, f.graph.paper.area, 1e-6)
  }

  test("Kawasaki is exactly the promise that the folding map is well defined") {
    val cp = CreasePattern.square(200)
      .crease(Pt(0, 0), Pt(200, 200), Assignment.Mountain)
      .crease(Pt(100, 0), Pt(100, 200), Assignment.Valley)
      .crease(Pt(0, 100), Pt(200, 100), Assignment.Valley)
      .crease(Pt(200, 0), Pt(60, 200), Assignment.Mountain)  // breaks Kawasaki
    val g = cp.planarize
    assert(Laws.checkGeometry(g).exists { case _: Violation.Kawasaki => true; case _ => false },
      "expected a Kawasaki violation to set the scene")
    assert(FoldedState.from(g).isLeft, "an inconsistent pattern must not produce a folded state")
  }

  test("Maekawa falls out of the stacking, without being told") {
    val g = Patterns.impossibleX(200).pattern.planarize
    val state = FoldedState.from(g).toOption.get
    assertEquals(Layers.solve(state).ok, false)
  }

  test("a local rule can be right everywhere and wrong overall") {
    val uniform = Patterns.uniformRule
    val g = uniform.pattern.planarize
    assertEquals(Laws.checkAll(g), Vector.empty, "every interior vertex must pass")
    val state = FoldedState.from(g).toOption.get
    assertEquals(Layers.solve(state).ok, false, "and there must still be no stacking")
  }

  test("the same rule does fold on a smaller sheet") {
    val small = Patterns.waterbombTessellation(3, 3, 44, uniform = true)
    val g = small.pattern.planarize
    assertEquals(Laws.checkAll(g), Vector.empty)
    assert(FoldedState.from(g).toOption.exists(s => Layers.solve(s).ok),
      "3x3 folds, 4x4 does not: there is no size at which local checking becomes enough")
  }

  test("dropping a condition makes the checker accept the impossible") {
    val state = FoldedState.from(Patterns.uniformRule.pattern.planarize).toOption.get
    assertEquals(Layers.solve(state).ok, false)
    val without = Layers.solve(state, conditions = Layers.Conditions(tacoTaco = false))
    assertEquals(without.ok, true, "taco-taco is what rules this one out")
  }

  test("unassigned creases get mountain or valley from the layer order") {
    val m = fold(Patterns.yoshimura())
    assert(m.graph.edges.forall(e => !e.assignment.isUndecided), "every crease is labelled")
    assertEquals(Laws.checkAll(m.graph), Vector.empty)
  }

  test("only a crease marked optional may stay flat") {
    val size = 200.0
    val cp = CreasePattern.square(size)
      .crease(Pt(0, 0), Pt(size, size), Assignment.Unassigned)
      .crease(Pt(size, 0), Pt(0, size), Assignment.Unassigned)
      .crease(Pt(size / 2, 0), Pt(size / 2, size), Assignment.Optional)
      .crease(Pt(0, size / 2), Pt(size, size / 2), Assignment.Optional)
    val g = cp.planarize
    val labellings = Assigner.choices(g).toVector
    def flatAt(l: PlanarGraph, from: Assignment) =
      l.edges.indices.filter(i => g.edges(i).assignment == from).map(l.edges(_).assignment)
    assert(labellings.exists(l => flatAt(l, Assignment.Optional).contains(Assignment.Flat)), "a midline can stay flat")
    labellings.foreach: l =>
      assert(flatAt(l, Assignment.Unassigned).forall(_ == Assignment.Unassigned), "a diagonal must fold")
    assert(labellings.map(FoldedModel.of(_)).exists(_.toOption.exists(_.thickness == 4)),
      "one midline flat: the waterbomb base")
  }

  test("ranked search: fewest layers first") {
    val m = Patterns.birdLines()
    val ranked = FoldedModel.searchAll(m.pattern.planarize, m.symmetries, rankMillis = 1000)
      .fold(why => fail(why.explain), identity)
    val thickness = ranked.models.map(_.thickness)
    assertEquals(thickness, thickness.sorted)
    assert(ranked.best.graph.edges.exists(_.assignment == Assignment.Flat), "the thinnest leaves something flat")
  }

  test("a flat crease on a fold's line is paper too: no layer slips through it") {
    val m = Patterns.birdLines()
    val ranked = FoldedModel.searchAll(m.pattern.planarize, m.symmetries, rankMillis = 1000)
      .fold(why => fail(why.explain), identity)
    ranked.models.foreach: f =>
      val st = f.state
      val layer = f.stacking.layerOf
      val eps = math.sqrt(st.graph.paper.area) * 1e-6
      val creases = st.graph.edges.indices.flatMap: e =>
        val (kind, sides) = (st.graph.assignment(e), st.faces.facesAt(e))
        Option.when((kind.isFolded || kind == Assignment.Flat) && sides.length == 2):
          (st.maps(sides(0))(st.faces.edgeSeg(e)), sides(0), sides(1), kind.isFolded)
      def side(l: Line, x: Int) = math.signum(l.signedDist(st.folded(x).centroid))
      def between(x: Int, a: Int, b: Int) = layer(x) > (layer(a) min layer(b)) && layer(x) < (layer(a) max layer(b))
      for
        (s1, a, b, fold1) <- creases
        (s2, c, d, fold2) <- creases
        if Set(a, b, c, d).size == 4 && s1.line.sameAs(s2.line) && s1.collinearOverlap(s2, eps).isDefined
      do
        val l = s1.line
        val crosses = (fold1, fold2) match
          case (true, true)  => side(l, a) == side(l, c) && between(c, a, b) != between(d, a, b)
          case (true, false) => between(if side(l, c) == side(l, a) then c else d, a, b)
          case (false, true) => false // the (true, false) case, seen from the other crease
          case (false, false) =>
            val (c1, d1) = if side(l, a) == side(l, c) then (c, d) else (d, c)
            (layer(a) < layer(c1)) != (layer(b) < layer(d1))
        assert(!crosses, s"paper crosses itself where creases ($a,$b) and ($c,$d) meet at ${s1.midpoint}")
  }

  test("a FoldedModel cannot be built for something that does not fold") {
    assert(FoldedModel.of(Patterns.hypar().pattern.planarize).isLeft)
    assert(FoldedModel.of(Patterns.impossibleX().pattern.planarize).isLeft)
    assert(FoldedModel.of(Patterns.deadEnd().pattern.planarize).isLeft)
  }
