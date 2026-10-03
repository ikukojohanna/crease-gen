package origami.tiling

import origami.folding.FoldedModel
import origami.geometry.{Geometry, Tol, Vec}
import origami.laws.Laws
import origami.tiling.ShrinkRotate.Problem

class TilingSpec extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)

  private def regular(size: Int): Vector[(String, Tiling)] = Vector(
    "6.6.6" -> Tilings.hexagonal(size), "4.4.4.4" -> Tilings.square(size),
    "3.3.3.3.3.3" -> Tilings.triangular(size), "3.6.3.6" -> Tilings.trihexagonal(size))

  private def twists(t: Tiling, s: Double, degrees: Double) =
    ShrinkRotate(t, s, Geometry.degrees(degrees)).fold(p => fail(p.explain), identity)

  test("regular tilings with centroids as centres are spider webs") {
    for size <- 1 to 3; (name, t) <- regular(size) do
      assertEquals(Reciprocal.check(t), Vector.empty, s"$name, size $size")
  }

  test("a centre off the perpendicular is reported, edge by edge") {
    val t = Tilings.square(1)
    val nudged = Tiling.of(
      t.tiles.indices.toVector.map(i => (t.polygon(i), if i == 0 then t.tiles(i).centre + Vec(1, 0)
        else t.tiles(i).centre)), t.paper)
    val bad = Reciprocal.check(nudged)
    assert(bad.nonEmpty && bad.length <= 4, s"only the nudged tile's edges should break: ${bad.length}")
    assert(ShrinkRotate(nudged, 0.5, Geometry.degrees(30)).left.exists(_.isInstanceOf[Problem.NotSpiderWeb]))
  }

  test("every interior vertex of a regular tiling has a full ring of tiles") {
    for (name, t) <- regular(1) do
      val inside = t.points.indices.filter(p => t.paper.contains(t.points(p)))
      assert(inside.forall(t.isInterior), s"$name: a vertex on the paper lacks tiles")
  }

  test("pleats are parallelograms, as wide as the centres' distance times (cos alpha - s)") {
    val (s, alpha) = (0.55, Geometry.degrees(25))
    for (name, t) <- regular(1); e <- t.sharedEdges do
      val Vector(lu, lv, rv, ru) = ShrinkRotate.pleat(t, e, s, alpha): @unchecked
      assert(((lv - lu) - (rv - ru)).isZero, s"$name: opposite creases differ")
      assert(((ru - lu) - (rv - lv)).isZero, s"$name: opposite twist edges differ")
      val d = t.tiles(e.right).centre.distTo(t.tiles(e.left).centre)
      val width = math.abs((ru - lu).cross((lv - lu).normalized))
      assertEqualsDouble(width, d * (math.cos(alpha) - s), 1e-9, name)
  }

  test("shrink-and-rotate satisfies the local laws, either way round") {
    for (name, t) <- regular(2); (s, degrees) <- Vector((0.5, 30.0), (0.3, 45.0), (0.7, 20.0), (0.5, -30.0)) do
      assertEquals(Laws.checkAll(twists(t, s, degrees).planarize), Vector.empty, s"$name, s=$s, alpha=$degrees")
  }

  test("small instances fold") {
    for (name, t) <- regular(1); (s, degrees) <- Vector((0.5, 30.0), (0.6, 40.0), (0.5, -30.0)) do
      val folded = FoldedModel.of(twists(t, s, degrees))
      assert(folded.isRight, s"$name, s=$s, alpha=$degrees: ${folded.left.map(_.explain)}")
  }

  test("parameters that cannot fold are rejected, not built") {
    val t = Tilings.trihexagonal(1)
    def problem(s: Double, degrees: Double) = ShrinkRotate(t, s, Geometry.degrees(degrees)).left.toOption
    assert(problem(0, 30).exists(_.isInstanceOf[Problem.Scale]))
    assert(problem(1.2, 30).exists(_.isInstanceOf[Problem.Scale]))
    assert(problem(0.5, 0).exists(_.isInstanceOf[Problem.Twist]))
    assert(problem(0.5, 95).exists(_.isInstanceOf[Problem.Twist]))
    assert(problem(0.9, 30).exists(_.isInstanceOf[Problem.PleatsFlip]), "cos 30 = 0.87 < 0.9")
    assert(problem(0.5, 10).exists(_.isInstanceOf[Problem.PleatTooBlunt]), "pleat angle 70 deg beats the 60 deg corners")
  }

  test("the pleat-angle limit sits at the smallest corner angle") {
    // On 6.6.6 every corner is 120 degrees, so the limit is 60.
    val t = Tilings.hexagonal(1)
    val s = 0.5
    def alphaFor(gammaDeg: Double): Double =
      // Solve atan2(cos a - s, sin a) = gamma for a by bisection; the pleat angle falls as a grows.
      Iterator.iterate((1e-6, math.Pi / 2 - 1e-6)) { (lo, hi) =>
        val mid = (lo + hi) / 2
        if ShrinkRotate.pleatAngle(s, mid) > Geometry.degrees(gammaDeg) then (mid, hi) else (lo, mid)
      }.drop(60).next()._1
    assert(ShrinkRotate(t, s, alphaFor(59.5)).isRight)
    assert(ShrinkRotate(t, s, alphaFor(60.5)).isLeft)
  }
