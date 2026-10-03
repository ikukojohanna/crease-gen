package origami.tiling

import origami.geometry.{Bounds, Pt}

/** A Delaunay triangulation, built point by point (Bowyer-Watson). Triangles are counter-clockwise index triples. */
final case class Delaunay private (points: Vector[Pt], triangles: Vector[Delaunay.Triangle], hull: Set[Int]):

  /** The centre of the circle through a triangle's corners: a Voronoi vertex. */
  def circumcentre(t: Delaunay.Triangle): Pt = Delaunay.circumcentre(points(t.a), points(t.b), points(t.c))

object Delaunay:

  final case class Triangle(a: Int, b: Int, c: Int):
    def corners: Vector[Int] = Vector(a, b, c)
    def edges: Vector[(Int, Int)] = Vector((a, b), (b, c), (c, a))

  def apply(points: Vector[Pt]): Delaunay =
    val n = points.length
    val all = points ++ superTriangle(points)
    def circle(t: Triangle): (Pt, Double) =
      val c = circumcentre(all(t.a), all(t.b), all(t.c))
      (c, (all(t.a) - c).normSq)

    val start = Vector(Triangle(n, n + 1, n + 2)).map(t => (t, circle(t)))
    val built = points.indices.foldLeft(start): (tris, i) =>
      val p = all(i)
      val (bad, good) = tris.partition((_, c) => (p - c._1).normSq < c._2)
      // The cavity's rim: edges of exactly one bad triangle, each joined to the new point.
      val rim = bad.flatMap(_._1.edges).groupBy(e => (math.min(e._1, e._2), math.max(e._1, e._2)))
        .values.collect { case Vector(e) => e }
      good ++ rim.map((u, v) => Triangle(u, v, i)).map(t => (t, circle(t)))

    val (real, touchingSuper) = built.map(_._1).partition(_.corners.forall(_ < n))
    Delaunay(points, real, touchingSuper.flatMap(_.corners).filter(_ < n).toSet)

  def circumcentre(a: Pt, b: Pt, c: Pt): Pt =
    val (bx, by, cx, cy) = (b.x - a.x, b.y - a.y, c.x - a.x, c.y - a.y)
    val d = 2 * (bx * cy - by * cx)
    val (b2, c2) = (bx * bx + by * by, cx * cx + cy * cy)
    Pt(a.x + (cy * b2 - by * c2) / d, a.y + (bx * c2 - cx * b2) / d)

  /** A counter-clockwise triangle far round every point. */
  private def superTriangle(points: Vector[Pt]): Vector[Pt] =
    val b = Bounds.of(points)
    val (mid, span) = (b.lo.midpoint(b.hi), math.max(b.width, b.height) max 1.0)
    Vector(Pt(mid.x - 20 * span, mid.y - 10 * span), Pt(mid.x + 20 * span, mid.y - 10 * span),
      Pt(mid.x, mid.y + 20 * span))
