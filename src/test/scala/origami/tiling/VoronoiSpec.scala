package origami.tiling

import origami.folding.FoldedModel
import origami.geometry.{Geometry, Pt, Tol}
import origami.laws.Laws

class VoronoiSpec extends munit.FunSuite:
  given tol: Tol = Tol(1e-7)

  private val scattered: Vector[Pt] =
    val rnd = scala.util.Random(11)
    Vector.fill(60)(Pt(rnd.nextDouble() * 100, rnd.nextDouble() * 100))

  private val small = Graded.Params(paperRadius = 60, spacing = 20, focus = Pt(10, 5), focusRadius = 40, depth = 1)

  test("Delaunay: triangles turn counter-clockwise and no point lies inside any circumcircle") {
    val d = Delaunay(scattered)
    assert(d.triangles.nonEmpty)
    d.triangles.foreach: t =>
      val (a, b, c) = (scattered(t.a), scattered(t.b), scattered(t.c))
      assert((b - a).cross(c - a) > 0, s"$t is clockwise")
      val centre = d.circumcentre(t)
      val r = centre.distTo(a)
      scattered.indices.filterNot(t.corners.contains).foreach: i =>
        assert(scattered(i).distTo(centre) >= r - 1e-9, s"point $i inside the circumcircle of $t")
  }

  test("Voronoi cells with their sites as centres are a spider web") {
    val t = Voronoi.tiling(scattered, Graded.paper(small), _ => true)
    assert(t.tiles.nonEmpty)
    assertEquals(Reciprocal.check(t), Vector.empty)
  }

  test("a refinement step only adds points, all of the new level, all inside its disc") {
    val base = Graded.Level(Graded.sites(small.copy(depth = 0)), small.spacing, small.focusRadius, 0)
    val next = Graded.refine(small.focus, small.shrink)(base)
    assertEquals(next.sites.take(base.sites.length), base.sites)
    val added = next.sites.drop(base.sites.length)
    assert(added.nonEmpty)
    assert(added.forall((p, level) => level == 1 && p.distTo(small.focus) <= small.focusRadius + 1e-9))
    assertEquals(next.spacing, small.spacing / 2)
  }

  test("pruning leaves no narrow pleat and no corner too tight, and drops only refinement points") {
    val (s, alpha, minWidth) = (0.55, Geometry.degrees(30), 2.0)
    val pruned = Graded.pruned(small.copy(depth = 2), s, alpha, minWidth)
    assertEquals(ShrinkRotate.narrowPleats(pruned.tiling, s, alpha, minWidth), Vector.empty)
    val gamma = ShrinkRotate.pleatAngle(s, alpha)
    assert(pruned.tiling.interiorCorners.forall(c => !ShrinkRotate.cornerTooTight(c.angle, gamma)))
    val base = Graded.sites(small.copy(depth = 0)).map(_._1).toSet
    assert(pruned.dropped.forall(p => !base(p)), "a level-0 point was dropped")
  }

  test("a small graded Voronoi tessellation passes the local laws and folds") {
    val (s, alpha) = (0.55, Geometry.degrees(30))
    val t = Graded.pruned(small, s, alpha, minWidth = 1.5).tiling
    val cp = ShrinkRotate(t, s, alpha).fold(p => fail(p.explain), identity)
    assertEquals(Laws.checkAll(cp.planarize), Vector.empty)
    assert(FoldedModel.of(cp).isRight)
  }
