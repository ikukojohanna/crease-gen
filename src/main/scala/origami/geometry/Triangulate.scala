package origami.geometry

import origami.utils.SeqUtils.*

import scala.collection.mutable

object Triangulate:

  def apply(poly: Polygon)(using tol: Tol): Vector[Tri] =
    val vs = poly.ccw.vertices
    if vs.length < 3 then Vector.empty
    else
      val idx = mutable.ArrayBuffer.from(vs.indices)
      val out = mutable.ArrayBuffer.empty[Tri]
      var stuck = false
      while idx.length > 3 && !stuck do
        val corners = idx.toVector.cyclicTriples.map((i, j, k) => Tri(vs(i), vs(j), vs(k)))
        corners.indexWhere(isEar(_, idx.map(vs))) match
          case -1 => stuck = true  // no ear left: finish with a fan
          case k =>
            out += corners(k)
            idx.remove(k)
      if idx.length == 3 then out += Tri(vs(idx(0)), vs(idx(1)), vs(idx(2)))
      else if idx.length > 3 then
        for k <- 1 until idx.length - 1 do out += Tri(vs(idx(0)), vs(idx(k)), vs(idx(k + 1)))
      out.filter(_.area > tol.value).toVector

  private def isEar(t: Tri, remaining: Iterable[Pt])(using tol: Tol): Boolean =
    val convex = (t.b - t.a).cross(t.c - t.b) > tol.value
    convex && !remaining.filterNot(t.vertices.contains).exists(strictlyInside(_, t))

  private def strictlyInside(p: Pt, t: Tri)(using tol: Tol): Boolean =
    val s1 = (t.b - t.a).cross(p - t.a)
    val s2 = (t.c - t.b).cross(p - t.b)
    val s3 = (t.a - t.c).cross(p - t.c)
    (s1 > tol.value && s2 > tol.value && s3 > tol.value) ||
      (s1 < -tol.value && s2 < -tol.value && s3 < -tol.value)
