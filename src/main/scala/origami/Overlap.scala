package origami

import scala.collection.mutable

final case class Tri(a: Pt, b: Pt, c: Pt):
  def area: Double = math.abs((b - a).cross(c - a)) / 2
  def vertices: Vector[Pt] = Vector(a, b, c)
  def edges: Vector[(Pt, Pt)] = Vector((a, b), (b, c), (c, a))
  def centroid: Pt = Pt((a.x + b.x + c.x) / 3, (a.y + b.y + c.y) / 3)

/** Cut a facet into triangles so that "do these two pieces of paper overlap?"
  * becomes a question with a cheap, exact answer.
  *
  * Ear clipping: repeatedly find a corner whose triangle is inside the polygon
  * and contains no other vertex, and snip it off.
  */
object Triangulate:

  def apply(poly: Polygon)(using tol: Tol): Vector[Tri] =
    val vs = poly.ccw.vertices
    if vs.length < 3 then Vector.empty
    else
      val idx = mutable.ArrayBuffer.from(vs.indices)
      val out = mutable.ArrayBuffer.empty[Tri]
      var guard = 0
      while idx.length > 3 && guard < 4 * vs.length do
        val ear = idx.indices.find: k =>
          val (p, q, r) = (vs(idx((k + idx.length - 1) % idx.length)), vs(idx(k)),
            vs(idx((k + 1) % idx.length)))
          val convex = (q - p).cross(r - q) > tol.value
          convex && !idx.filterNot(i => vs(i) == p || vs(i) == q || vs(i) == r)
            .exists(i => strictlyInside(vs(i), Tri(p, q, r)))
        ear match
          case Some(k) =>
            val (p, q, r) = (vs(idx((k + idx.length - 1) % idx.length)), vs(idx(k)),
              vs(idx((k + 1) % idx.length)))
            out += Tri(p, q, r)
            idx.remove(k)
            guard = 0
          case None => guard = 4 * vs.length // degenerate: fall back to a fan
      if idx.length == 3 then out += Tri(vs(idx(0)), vs(idx(1)), vs(idx(2)))
      else if idx.length > 3 then
        for k <- 1 until idx.length - 1 do out += Tri(vs(idx(0)), vs(idx(k)), vs(idx(k + 1)))
      out.filter(_.area > tol.value).toVector

  private def strictlyInside(p: Pt, t: Tri)(using tol: Tol): Boolean =
    val s1 = (t.b - t.a).cross(p - t.a)
    val s2 = (t.c - t.b).cross(p - t.b)
    val s3 = (t.a - t.c).cross(p - t.c)
    (s1 > tol.value && s2 > tol.value && s3 > tol.value) ||
      (s1 < -tol.value && s2 < -tol.value && s3 < -tol.value)

/** Does this piece of paper lie on that one?
  *
  * Every question the layer ordering asks reduces to one of these two: do two
  * facets share area, and does a crease pass through the middle of a facet.
  * Both need a real tolerance rather than an exact one -- two facets that meet
  * edge to edge are touching, not overlapping, and the difference decides
  * whether the ordering is constrained at all.
  */
object Overlap:

  /** Convex separating-axis test, strict: sharing only a boundary is not
    * overlapping. */
  def triangles(t1: Tri, t2: Tri, eps: Double): Boolean =
    !separated(t1, t2, eps) && !separated(t2, t1, eps)

  private def separated(t1: Tri, t2: Tri, eps: Double): Boolean =
    t1.edges.exists: (p, q) =>
      val n = (q - p).perp
      val len = n.norm
      if len < 1e-12 then false
      else
        val axis = n / len
        val (a1, a2) = (t1.vertices.map(v => (v - Pt.origin).dot(axis)),
          t2.vertices.map(v => (v - Pt.origin).dot(axis)))
        a1.max <= a2.min + eps || a2.max <= a1.min + eps

  /** The part of `s` strictly inside the triangle, if it is long enough to
    * matter. */
  def segmentThrough(s: Seg, t: Tri, eps: Double): Boolean =
    var lo = 0.0
    var hi = 1.0
    var ok = true
    for (p, q) <- t.edges if ok do
      // inward normal of a counter-clockwise triangle
      val n = (q - p).perp
      val d0 = (s.a - p).dot(n)
      val d1 = (s.b - p).dot(n)
      val diff = d1 - d0
      if math.abs(diff) < 1e-15 then
        if d0 < 0 then ok = false
      else
        val t0 = -d0 / diff
        if diff > 0 then lo = math.max(lo, t0) else hi = math.min(hi, t0)
      if lo > hi then ok = false
    ok && (hi - lo) * s.length > eps && strictlyInsideTri(s.at((lo + hi) / 2), t, eps)

  private def strictlyInsideTri(p: Pt, t: Tri, eps: Double): Boolean =
    t.edges.forall((a, b) => (p - a).cross(b - a) < -eps * (b - a).norm)
