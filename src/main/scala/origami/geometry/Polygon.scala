package origami.geometry

import origami.utils.SeqUtils.*

import scala.math.{abs, Pi}

final case class Polygon(vertices: Vector[Pt]):
  require(vertices.length >= 3, "a sheet of paper needs at least three corners")

  def edges: Vector[Seg] = vertices.cyclicPairs.map((p, q) => Seg(p, q))

  def signedArea: Double =
    vertices.cyclicPairs.map((p, q) => p.x * q.y - q.x * p.y).sum / 2

  def area: Double = abs(signedArea)

  def ccw: Polygon = if signedArea < 0 then Polygon(vertices.reverse) else this

  def bounds: Bounds = Bounds.of(vertices)

  def onBoundary(p: Pt)(using Tol): Boolean = edges.exists(_.contains(p))

  def centroid: Pt =
    val n = vertices.length
    var (cx, cy, a) = (0.0, 0.0, 0.0)
    for (p, q) <- vertices.cyclicPairs do
      val cross = p.x * q.y - q.x * p.y
      a += cross
      cx += (p.x + q.x) * cross
      cy += (p.y + q.y) * cross
    if math.abs(a) < 1e-12 then Pt(vertices.map(_.x).sum / n, vertices.map(_.y).sum / n)
    else Pt(cx / (3 * a), cy / (3 * a))

  def contains(p: Pt)(using tol: Tol): Boolean =
    onBoundary(p) || {
      val crossings = vertices.cyclicPairs.count: (pj, pi) =>
        (pi.y > p.y) != (pj.y > p.y) && p.x < (pj.x - pi.x) * (p.y - pi.y) / (pj.y - pi.y) + pi.x
      crossings % 2 == 1
    }

  def clip(l: Line)(using tol: Tol): Vector[Seg] =
    val ts = (edges.flatMap(e => l.intersect(e.line).filter(e.contains)).map(l.paramOf) ++
      vertices.filter(l.contains).map(l.paramOf)).sorted
    ts.distinctConsecutiveWith(tol.eqv)
      .sliding(2)
      .collect { case Vector(t0, t1) if contains(l.at((t0 + t1) / 2)) => Seg(l.at(t0), l.at(t1)) }
      .filterNot(_.isDegenerate)
      .toVector

object Polygon:
  def square(side: Double): Polygon =
    Polygon(Vector(Pt(0, 0), Pt(side, 0), Pt(side, side), Pt(0, side)))

  def rectangle(w: Double, h: Double): Polygon =
    Polygon(Vector(Pt(0, 0), Pt(w, 0), Pt(w, h), Pt(0, h)))

  def regular(n: Int, radius: Double, centre: Pt = Pt.origin): Polygon =
    Polygon(Vector.tabulate(n)(i => centre + Vec.polar(radius, 2 * Pi * i / n)))
