package origami

import scala.math.Pi

/** The axioms are specified by what the paper does, so the tests say exactly
  * that: after the fold, is the point where it was supposed to land?
  */
class AxiomSuite extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)
  private val loose = Tol(1e-6)

  private def landsOn(l: Line, p: Pt, target: Line, t: Tol = tol): Boolean =
    t.isZero(target.signedDist(l.reflect(p)))

  test("reflecting twice is doing nothing") {
    val l = Line.through(Pt(1, 2), Pt(4, -3))
    val p = Pt(7, 11)
    assert(l.reflect(l.reflect(p)) ~= p)
  }

  test("a fold preserves distance to the crease") {
    val l = Line.through(Pt(0, 0), Pt(1, 3))
    val p = Pt(5, -2)
    assertEqualsDouble(math.abs(l.signedDist(p)), math.abs(l.signedDist(l.reflect(p))), 1e-12)
  }

  test("O1 passes through both points") {
    val (p, q) = (Pt(1, 1), Pt(5, 3))
    val l = Axioms.o1(p, q).get
    assert(l.contains(p) && l.contains(q))
  }

  test("O2 puts one point exactly on the other") {
    val (p, q) = (Pt(1, 1), Pt(5, 3))
    val l = Axioms.o2(p, q).get
    assert(l.reflect(p) ~= q)
  }

  test("O3 bisects: it folds one line onto the other") {
    val l1 = Line.through(Pt(0, 0), Pt(1, 0))
    val l2 = Line.through(Pt(0, 0), Pt(1, 1))
    val folds = Axioms.o3(l1, l2)
    assertEquals(folds.length, 2)
    folds.foreach: f =>
      val moved = Line(f.reflect(l1.origin), f.reflect(l1.dir))
      assert(moved.sameAs(l2), s"$f did not fold l1 onto l2")
  }

  test("O3 on parallels gives the midline") {
    val l1 = Line(Pt(0, 0), Vec(1, 0))
    val l2 = Line(Pt(0, 4), Vec(1, 0))
    val folds = Axioms.o3(l1, l2)
    assertEquals(folds.length, 1)
    assert(folds.head.contains(Pt(3, 2)))
  }

  test("O4 is perpendicular and through the point") {
    val l = Line.through(Pt(0, 0), Pt(2, 1))
    val p = Pt(3, 7)
    val f = Axioms.o4(p, l)
    assert(f.contains(p))
    assert(tol.isZero(f.dir.dot(l.dir)))
  }

  test("O5 lands the point on the line and passes through the reference") {
    val p = Pt(0, 3)
    val through = Pt(2, 2)
    val l = Line(Pt(0, 0), Vec(1, 0))
    val folds = Axioms.o5(p, through, l)
    assert(folds.nonEmpty)
    folds.foreach: f =>
      assert(f.contains(through), "crease missed its reference point")
      assert(landsOn(f, p, l), "point did not land on the line")
  }

  test("O5 has no answer when the circle misses the line") {
    val folds = Axioms.o5(Pt(0, 10), Pt(0, 9), Line(Pt(0, 0), Vec(1, 0)))
    assertEquals(folds, Nil)
  }

  test("O6 places both points at once -- the cubic fold") {
    val p1 = Pt(1, 1)
    val p2 = Pt(4, 3)
    val l1 = Line(Pt(0, 0), Vec(1, 0))
    val l2 = Line(Pt(0, 0), Vec(0, 1))
    val folds = Axioms.o6(p1, l1, p2, l2)
    assert(folds.nonEmpty, "expected at least one common tangent")
    assert(folds.length <= 3, s"a cubic has at most three roots, got ${folds.length}")
    folds.foreach: f =>
      assert(landsOn(f, p1, l1, loose), "p1 missed l1")
      assert(landsOn(f, p2, l2, loose), "p2 missed l2")
  }

  test("O7 lands the point and is perpendicular to the reference line") {
    val p = Pt(2, 5)
    val l1 = Line(Pt(0, 0), Vec(1, 0))
    val l2 = Line(Pt(0, 0), Vec(0, 1))
    val f = Axioms.o7(p, l1, l2).get
    assert(tol.isZero(f.dir.dot(l2.dir)), "crease not perpendicular to l2")
    assert(landsOn(f, p, l1))
  }

  test("O7 fails when the required crease direction cannot reach the line") {
    assertEquals(Axioms.o7(Pt(2, 5), Line(Pt(0, 0), Vec(1, 0)), Line(Pt(0, 0), Vec(1, 0))), None)
  }

  test("angles normalise onto the circle") {
    assertEqualsDouble(Geometry.norm2Pi(-Pi / 2), 3 * Pi / 2, 1e-12)
    assertEqualsDouble(Geometry.norm2Pi(5 * Pi), Pi, 1e-12)
  }
