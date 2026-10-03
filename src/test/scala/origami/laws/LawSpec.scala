package origami.laws

import origami.folding.FoldedModel
import origami.geometry.Tol
import origami.library.{Model, Patterns}
import origami.pattern.{Assignment, PlanarGraph}

class LawSpec extends munit.FunSuite:
  given Tol = Tol(1e-7)

  private def decide(m: Model): PlanarGraph =
    val g = m.pattern.planarize
    if g.edges.exists(_.assignment.isUndecided) then
      FoldedModel.searchAll(g, m.symmetries, rankMillis = 0).map(_.best.graph).getOrElse(g)
    else g

  private val foldable = Vector(
    Patterns.accordion(), Patterns.miura(), Patterns.yoshimura(),
    Patterns.waterbombBase(), Patterns.preliminaryBase(), Patterns.birdBase(),
    Patterns.waterbombTessellation())

  foldable.foreach: m =>
    test(s"${m.name} satisfies the local laws") {
      FlatFoldable.from(decide(m)) match
        case Right(_) => ()
        case Left(vs) => fail(vs.map(_.explain).mkString("\n"))
    }

  test("Maekawa: every interior vertex has even degree") {
    foldable.foreach: m =>
      decide(m).interiorVertices.foreach: v =>
        assertEquals(v.degree % 2, 0, s"${m.name} at ${v.at}")
  }

  test("Maekawa: mountains minus valleys is always plus or minus two") {
    foldable.foreach: m =>
      val g = decide(m)
      g.interiorVertices.filter(_.degree > 0).foreach: v =>
        val kinds = v.foldedEdges.map(e => g.edges(e).assignment)
        val diff = kinds.count(_ == Assignment.Mountain) - kinds.count(_ == Assignment.Valley)
        assertEquals(math.abs(diff), 2, s"${m.name} at ${v.at}")
  }

  test("four creases at the centre of a square cannot all be mountains") {
    val vs = Laws.checkAll(Patterns.impossibleX().pattern.planarize)
    assert(vs.exists { case _: Violation.Maekawa => true; case _ => false })
  }

  test("a crease cannot stop in the middle of the sheet") {
    val vs = Laws.checkAll(Patterns.deadEnd().pattern.planarize)
    assert(vs.exists { case _: Violation.DeadEnd => true; case _ => false })
  }

  test("the classic hypar is rejected, and that is correct") {
    val vs = Laws.checkAll(Patterns.hypar().pattern.planarize)
    assert(vs.nonEmpty, "the hypar is not flat-foldable and must not pass")
    assert(vs.forall { case _: Violation.Maekawa => true; case _ => false })
  }

  test("the solver respects creases that are already decided") {
    val m = Patterns.birdLines()
    val g = m.pattern.planarize
    val pinned = g.withAssignments((_, e) =>
      if e.assignment == Assignment.Unassigned && e.u == 0 then Assignment.Mountain else e.assignment)
    val fixed = pinned.edges.zipWithIndex.collect { case (e, i) if e.assignment.isFolded => i }
    FoldedModel.searchAll(pinned, rankMillis = 0).foreach: solved =>
      fixed.foreach(i => assertEquals(solved.best.graph.edges(i).assignment, pinned.edges(i).assignment))
  }

  test("FlatFoldable can only be built by passing the laws") {
    assert(FlatFoldable.from(Patterns.impossibleX().pattern).isLeft)
    assert(FlatFoldable.from(decide(Patterns.miura())).isRight)
  }
