package origami.geometry

final case class Seg(a: Pt, b: Pt):
  def vec: Vec = b - a
  def length: Double = vec.norm
  def dir: Vec = vec.normalized
  def line: Line = Line.through(a, b)
  def midpoint: Pt = a.midpoint(b)
  def at(t: Double): Pt = a.lerp(b, t)
  def reverse: Seg = Seg(b, a)

  def isDegenerate(using tol: Tol): Boolean = tol.isZero(length)

  def contains(p: Pt)(using tol: Tol): Boolean =
    val v = vec
    val w = p - a
    tol.isZero(v.cross(w) / v.norm) && {
      val t = w.dot(v) / v.normSq
      t >= -tol.value && t <= 1 + tol.value
    }

  def paramOf(p: Pt): Double = (p - a).dot(vec) / vec.normSq

  /** None for parallel or collinear segments. */
  def intersect(o: Seg)(using tol: Tol): Option[Pt] =
    val (p, r, q, s) = (a, vec, o.a, o.vec)
    val denom = r.cross(s)
    if tol.isZero(denom) then None
    else
      val t = (q - p).cross(s) / denom
      val u = (q - p).cross(r) / denom
      val eps = tol.value / math.max(r.norm, s.norm)
      if t >= -eps && t <= 1 + eps && u >= -eps && u <= 1 + eps then Some(at(t))
      else None

  /** Assumes `o` lies on the same line. */
  def collinearOverlap(o: Seg, minLength: Double): Option[Seg] =
    val l = line
    val (t0, t1) = (l.paramOf(a), l.paramOf(b))
    val (u0, u1) = (l.paramOf(o.a), l.paramOf(o.b))
    val lo = math.max(math.min(t0, t1), math.min(u0, u1))
    val hi = math.min(math.max(t0, t1), math.max(u0, u1))
    Option.when(hi - lo > minLength)(Seg(l.at(lo), l.at(hi)))
