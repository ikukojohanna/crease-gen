package origami.folding

import origami.geometry.{Line, Pt, Rigid, Tol}
import origami.laws.{Assigner, Laws, Violation}
import origami.library.{Model, Patterns}
import origami.pattern.{Assignment, CreasePattern, Faces}

class FoldingSpec extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)

  private def fold(m: Model): FoldedModel =
    val g = m.pattern.planarize
    val r =
      if g.edges.exists(_.assignment == Assignment.Unassigned) then
        FoldedModel.search(g, m.symmetries, millis = 60000).map(_._1)
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

  test("the local laws are not enough, and the search is what closes the gap") {
    val g = Assigner.blank(Patterns.birdBase().pattern.planarize)
    val locallyValid = Assigner.labellings(g).take(40).toVector
    assert(locallyValid.length == 40)
    locallyValid.foreach(s => assertEquals(Laws.checkAll(s), Vector.empty))
    val folds = locallyValid.count(s => FoldedModel.of(s).isRight)
    assert(folds < locallyValid.length,
      "if every locally valid labelling folded, the layer solver would be pointless")
  }

  test("a FoldedModel cannot be built for something that does not fold") {
    assert(FoldedModel.of(Patterns.hypar().pattern.planarize).isLeft)
    assert(FoldedModel.of(Patterns.impossibleX().pattern.planarize).isLeft)
    assert(FoldedModel.of(Patterns.deadEnd().pattern.planarize).isLeft)
  }
