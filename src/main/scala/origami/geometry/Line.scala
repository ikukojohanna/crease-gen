package origami.geometry

final case class Line(origin: Pt, dir: Vec):
  def normal: Vec = dir.perp
  def signedDist(p: Pt): Double = (p - origin).dot(normal)
  def contains(p: Pt)(using tol: Tol): Boolean = tol.isZero(signedDist(p))
  def paramOf(p: Pt): Double = (p - origin).dot(dir)
  def at(t: Double): Pt = origin + dir * t
  def project(p: Pt): Pt = at(paramOf(p))

  def reflect(p: Pt): Pt = p - normal * (2 * signedDist(p))
  def reflect(v: Vec): Vec = v - normal * (2 * v.dot(normal))
  def reflect(s: Seg): Seg = Seg(reflect(s.a), reflect(s.b))

  def isParallelTo(o: Line)(using tol: Tol): Boolean = tol.isZero(dir.cross(o.dir))

  def sameAs(o: Line)(using tol: Tol): Boolean =
    isParallelTo(o) && contains(o.origin)

  def intersect(o: Line)(using tol: Tol): Option[Pt] =
    val d = dir.cross(o.dir)
    if tol.isZero(d) then None
    else Some(at((o.origin - origin).cross(o.dir) / d))

  def flip: Line = Line(origin, -dir)

object Line:
  def through(a: Pt, b: Pt): Line = Line(a, (b - a).normalized)

  def perpendicularBisector(a: Pt, b: Pt): Line =
    Line(a.midpoint(b), (b - a).normalized.perp)
