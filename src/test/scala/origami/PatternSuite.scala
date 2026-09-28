package origami

import scala.math.Pi

class PatternSuite extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)

  test("a crease pattern cannot hold a crease off the paper") {
    val cp = CreasePattern.square(100).crease(Pt(-50, 50), Pt(150, 50), Assignment.Mountain)
    assert(cp.creases.nonEmpty)
    cp.creases.foreach: c =>
      assert(cp.paper.contains(c.a) && cp.paper.contains(c.b), s"crease $c escaped the sheet")
  }

  test("a fold that misses the paper adds nothing") {
    val cp = CreasePattern.square(100).fold(Line(Pt(500, 500), Vec(1, 0)), Assignment.Valley)
    assertEquals(cp.creases.length, 0)
  }

  test("a degenerate crease is not a crease") {
    val cp = CreasePattern.square(100).crease(Pt(10, 10), Pt(10, 10), Assignment.Valley)
    assertEquals(cp.creases.length, 0)
  }

  test("turning the sheet over swaps every mountain and valley") {
    val cp = Patterns.miura().pattern
    val back = cp.flipOver
    cp.creases.zip(back.creases).foreach: (a, b) =>
      assertEquals(b.assignment, a.assignment.opposite)
    assertEquals(cp.flipOver.flipOver.creases, cp.creases)
  }

  test("crossing creases meet at a shared vertex after planarisation") {
    val cp = CreasePattern.square(100)
      .crease(Pt(0, 0), Pt(100, 100), Assignment.Mountain)
      .crease(Pt(100, 0), Pt(0, 100), Assignment.Valley)
    val g = cp.planarize
    val centre = g.vertices.find(_ ~= Pt(50, 50))
    assert(centre.isDefined, "the crossing did not become a vertex")
    assertEquals(g.incident(g.vertices.indexWhere(_ ~= Pt(50, 50))).length, 4)
  }

  test("coincident creases planarise to one edge") {
    val cp = CreasePattern.square(100)
      .crease(Pt(0, 50), Pt(100, 50), Assignment.Mountain)
      .crease(Pt(0, 50), Pt(100, 50), Assignment.Mountain)
    assertEquals(cp.planarize.edges.count(_.assignment == Assignment.Mountain), 1)
  }

  test("sectors around any vertex sum to a full turn") {
    val g = Patterns.birdBase().pattern.planarize
    Assigner.solve(g).getOrElse(g).interiorVertices.foreach: v =>
      assertEqualsDouble(v.sectors.sum, 2 * Pi, 1e-9)
  }

  test("a pleat alternates, so the sheet accordions instead of rolling") {
    // Parallel creases that all fold the same way spiral shut instead of
    // opening and closing. The laws cannot tell the difference -- both satisfy
    // Maekawa and fold flat to the same outline -- so this has to be checked
    // directly. A rolled Miura is not a Miura.
    val m = Patterns.miura(cols = 6, rows = 4)
    val zigzag = m.pattern.creases.filter(c => !tol.eqv(c.a.y, c.b.y))
    val bands = zigzag.groupBy(c => math.min(c.a.y, c.b.y))
    assert(bands.size >= 3, "expected several bands to check")
    bands.foreach: (y, creases) =>
      val across = creases.sortBy(c => math.min(c.a.x, c.b.x)).map(_.assignment)
      across.sliding(2).foreach:
        case Vector(x, y2) => assertNotEquals(x, y2, s"band at y=$y does not alternate: $across")
        case _             => ()
  }

  test("and so does the accordion") {
    val kinds = Patterns.accordion(8, 200).pattern.creases.sortBy(_.a.x).map(_.assignment)
    kinds.sliding(2).foreach:
      case Vector(a, b) => assertNotEquals(a, b)
      case _            => ()
  }

  test("clipping a line to the paper stays on the paper") {
    val p = Polygon.regular(6, 50)
    val l = Line(Pt(0, 0), Vec(1, 0.3).normalized)
    val pieces = p.clip(l)
    assert(pieces.nonEmpty)
    pieces.foreach(s => assert(p.contains(s.midpoint) && p.contains(s.a) && p.contains(s.b)))
  }
