package origami.geometry

import origami.utils.SeqUtils.*

/** The Huzita-Hatori axioms. */
object Axioms:

  /** O1: the fold through `p` and `q`. */
  def o1(p: Pt, q: Pt)(using tol: Tol): Option[Line] =
    if p ~= q then None else Some(Line.through(p, q))

  /** O2: the fold placing `p` onto `q`. */
  def o2(p: Pt, q: Pt)(using tol: Tol): Option[Line] =
    if p ~= q then None else Some(Line.perpendicularBisector(p, q))

  /** O3: the folds placing `l1` onto `l2`. */
  def o3(l1: Line, l2: Line)(using tol: Tol): List[Line] =
    l1.intersect(l2) match
      case Some(c) =>
        val (d1, d2) = (l1.dir, l2.dir)
        List(Line(c, (d1 + d2).normalized), Line(c, (d1 - d2).normalized))
      case None =>
        if l1.sameAs(l2) then Nil
        else
          val mid = l1.origin.midpoint(l2.project(l1.origin))
          List(Line(mid, l1.dir))

  /** O4: the fold through `p` perpendicular to `l`. */
  def o4(p: Pt, l: Line): Line = Line(p, l.normal)

  /** O5: the folds through `through` placing `p` onto `l`. */
  def o5(p: Pt, through: Pt, l: Line)(using tol: Tol): List[Line] =
    val r = through.distTo(p)
    val foot = l.project(through)
    val d = through.distTo(foot)
    if tol.gt(d, r) then Nil
    else
      val half = math.sqrt(math.max(0.0, r * r - d * d))
      val images = if tol.isZero(half) then List(foot) else List(foot + l.dir * half, foot - l.dir * half)
      images.flatMap(q => o2(p, q))

  /** O6: the folds placing `p1` onto `l1` and `p2` onto `l2` (a cubic). */
  def o6(p1: Pt, l1: Line, p2: Pt, l2: Line)(using tol: Tol): List[Line] =
    val scale = math.max(1.0, List(p1.distTo(l1.project(p1)), p2.distTo(l2.project(p2)),
      p1.distTo(p2)).max) * 8

    def foldFor(t: Double): Option[Line] =
      val q = l1.at(t)
      if q ~= p1 then None else Some(Line.perpendicularBisector(p1, q))

    def residual(t: Double): Option[Double] =
      foldFor(t).map(f => l2.signedDist(f.reflect(p2)))

    val samples = 4000
    val ts = (0 to samples).map(i => -scale + 2 * scale * i / samples)
    val roots = ts.zip(ts.tail).flatMap((a, b) => rootBetween(residual, a, b))
    roots.flatMap(foldFor).distinctWith(_.sameAs(_)).toList

  /** O7: the fold placing `p` onto `l1`, perpendicular to `l2`. */
  def o7(p: Pt, l1: Line, l2: Line)(using tol: Tol): Option[Line] =
    val n = l2.dir
    val denom = n.dot(l1.normal)
    if tol.isZero(denom) then None
    else
      val h = l1.signedDist(p) / (2 * denom)
      val crease = Line(p - n * h, l2.normal)
      if crease.contains(p) then None else Some(crease)

  /** A root of `f` in `[a, b]`, if `f` is zero at `a` or changes sign across the interval. */
  private def rootBetween(f: Double => Option[Double], a: Double, b: Double): Option[Double] =
    (f(a), f(b)) match
      case (Some(fa), Some(_)) if fa == 0.0           => Some(a)
      case (Some(fa), Some(fb)) if fa.sign != fb.sign => Some(bisect(f, a, b))
      case _                                          => None

  private def bisect(f: Double => Option[Double], lo: Double, hi: Double): Double =
    val signAtLo = f(lo).getOrElse(0.0).sign
    val (a, b) = (1 to 80).foldLeft((lo, hi)): (range, _) =>
      val (a, b) = range
      val mid = (a + b) / 2
      f(mid) match
        case Some(fm) if fm.sign != signAtLo => (a, mid)
        case _                               => (mid, b)
    (a + b) / 2
