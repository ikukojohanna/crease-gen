package origami

/** The Huzita-Hatori axioms: the complete instruction set of a sheet of paper.
  *
  * Each axiom is a total, pure function from references (points and lines that
  * already exist on the paper) to the folds those references determine. Nothing
  * is mutated and nothing is drawn; an axiom only says *which line*.
  *
  * The return types carry the mathematics. O1 and O2 determine at most one
  * fold, so they return `Option`. O3 and O5 may determine two, O6 up to three,
  * so they return `List`. The arity of the answer is part of the signature.
  */
object Axioms:

  /** O1: the fold through two points. */
  def o1(p: Pt, q: Pt)(using tol: Tol): Option[Line] =
    if p ~= q then None else Some(Line.through(p, q))

  /** O2: the fold placing `p` onto `q`. */
  def o2(p: Pt, q: Pt)(using tol: Tol): Option[Line] =
    if p ~= q then None else Some(Line.perpendicularBisector(p, q))

  /** O3: the folds placing line `l1` onto line `l2`.
    *
    * Two crossing lines have two bisectors; parallel lines have one midline.
    */
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

  /** O5: the folds through `through` that place `p` onto `l`.
    *
    * The image of `p` must lie on `l` and at an unchanged distance from
    * `through`: a circle meeting a line, hence zero, one or two answers.
    */
  def o5(p: Pt, through: Pt, l: Line)(using tol: Tol): List[Line] =
    val r = through.distTo(p)
    val foot = l.project(through)
    val d = through.distTo(foot)
    if tol.gt(d, r) then Nil
    else
      val half = math.sqrt(math.max(0.0, r * r - d * d))
      val images = if tol.isZero(half) then List(foot) else List(foot + l.dir * half, foot - l.dir * half)
      images.flatMap(q => o2(p, q))

  /** O6: the folds placing `p1` onto `l1` and `p2` onto `l2` simultaneously.
    *
    * This is the axiom that lifts paper past straightedge and compass: it is a
    * common tangent to two parabolas, and solving it means solving a cubic.
    * Paper trisects angles and doubles cubes because of this one fold.
    *
    * Parameterise candidate folds by where `p1` lands on `l1`; the residual
    * "how far is the image of `p2` from `l2`" is then a cubic in that
    * parameter, and we find its roots by sampling for sign changes and
    * bisecting. Up to three creases come back.
    */
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
    val roots = ts
      .sliding(2)
      .flatMap:
        case Seq(a, b) =>
          (residual(a), residual(b)) match
            case (Some(fa), Some(fb)) if fa == 0.0            => Some(a)
            case (Some(fa), Some(fb)) if fa.sign != fb.sign   => Some(bisect(residual, a, b))
            case _                                            => None
        case _ => None
      .toList

    roots.flatMap(foldFor).foldLeft(List.empty[Line]): (acc, l) =>
      if acc.exists(_.sameAs(l)) then acc else acc :+ l

  /** O7: the fold placing `p` onto `l1` with a crease perpendicular to `l2`. */
  def o7(p: Pt, l1: Line, l2: Line)(using tol: Tol): Option[Line] =
    val n = l2.dir // the crease runs perpendicular to l2, so this is its normal
    val denom = n.dot(l1.normal)
    if tol.isZero(denom) then None
    else
      val h = l1.signedDist(p) / (2 * denom)
      val crease = Line(p - n * h, l2.normal)
      if crease.contains(p) then None else Some(crease)

  private def bisect(f: Double => Option[Double], lo0: Double, hi0: Double): Double =
    var (lo, hi) = (lo0, hi0)
    val flo = f(lo).getOrElse(0.0)
    var i = 0
    while i < 80 do
      val mid = (lo + hi) / 2
      f(mid) match
        case Some(fm) if fm.sign == flo.sign => lo = mid
        case Some(_)                         => hi = mid
        case None                            => lo = mid
      i += 1
    (lo + hi) / 2
