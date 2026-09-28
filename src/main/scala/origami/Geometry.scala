package origami

import scala.math.{abs, atan2, hypot, Pi}

/** How close is close enough.
  *
  * Folding is a physical act, so equality is physical too: two points are the
  * same point when you could not tell them apart by creasing the paper. Making
  * the tolerance an explicit `using` parameter means no comparison in this
  * program can silently pretend to be exact.
  */
final case class Tol(value: Double):
  def isZero(a: Double): Boolean = abs(a) <= value
  def eqv(a: Double, b: Double): Boolean = abs(a - b) <= value
  def lt(a: Double, b: Double): Boolean = a < b - value
  def gt(a: Double, b: Double): Boolean = a > b + value

object Tol:
  given default: Tol = Tol(1e-7)

/** A displacement. Vectors add, scale and rotate; they have no position. */
final case class Vec(x: Double, y: Double):
  def +(o: Vec): Vec = Vec(x + o.x, y + o.y)
  def -(o: Vec): Vec = Vec(x - o.x, y - o.y)
  def *(s: Double): Vec = Vec(x * s, y * s)
  def /(s: Double): Vec = Vec(x / s, y / s)
  def unary_- : Vec = Vec(-x, -y)
  def dot(o: Vec): Double = x * o.x + y * o.y
  def cross(o: Vec): Double = x * o.y - y * o.x
  def normSq: Double = x * x + y * y
  def norm: Double = hypot(x, y)
  def normalized: Vec = this / norm
  /** Rotated a quarter turn counter-clockwise. */
  def perp: Vec = Vec(-y, x)
  def rotate(theta: Double): Vec =
    val (c, s) = (math.cos(theta), math.sin(theta))
    Vec(x * c - y * s, x * s + y * c)
  def angle: Double = Geometry.norm2Pi(atan2(y, x))
  def isZero(using tol: Tol): Boolean = tol.isZero(norm)

object Vec:
  val zero: Vec = Vec(0, 0)
  def polar(r: Double, theta: Double): Vec = Vec(r * math.cos(theta), r * math.sin(theta))

/** A location on the sheet. Points and vectors are deliberately different
  * types: `point + vector` is a point, `point - point` is a vector, and
  * `point + point` is nonsense the compiler will not let you write.
  */
final case class Pt(x: Double, y: Double):
  def +(v: Vec): Pt = Pt(x + v.x, y + v.y)
  def -(v: Vec): Pt = Pt(x - v.x, y - v.y)
  def -(o: Pt): Vec = Vec(x - o.x, y - o.y)
  def to(o: Pt): Vec = o - this
  def distTo(o: Pt): Double = to(o).norm
  def midpoint(o: Pt): Pt = Pt((x + o.x) / 2, (y + o.y) / 2)
  def lerp(o: Pt, t: Double): Pt = Pt(x + (o.x - x) * t, y + (o.y - y) * t)
  def ~=(o: Pt)(using tol: Tol): Boolean = tol.isZero(distTo(o))

object Pt:
  val origin: Pt = Pt(0, 0)

/** An infinite line: a base point and a unit direction.
  *
  * This is the type of a *fold*. Everything origami does to the plane, it does
  * by reflecting across one of these.
  */
final case class Line(origin: Pt, dir: Vec):
  /** Unit normal; `dir` rotated a quarter turn. */
  def normal: Vec = dir.perp
  def signedDist(p: Pt): Double = (p - origin).dot(normal)
  def contains(p: Pt)(using tol: Tol): Boolean = tol.isZero(signedDist(p))
  def paramOf(p: Pt): Double = (p - origin).dot(dir)
  def at(t: Double): Pt = origin + dir * t
  def project(p: Pt): Pt = at(paramOf(p))

  /** The one operation origami actually performs. */
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

  /** The crease you get by folding `a` onto `b`. Axiom O2. */
  def perpendicularBisector(a: Pt, b: Pt): Line =
    Line(a.midpoint(b), (b - a).normalized.perp)

/** A finite crease line: the part of a fold that is actually on the paper. */
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

  /** Parameter in [0,1] of the projection of `p`. */
  def paramOf(p: Pt): Double = (p - a).dot(vec) / vec.normSq

  /** Single crossing point, if the two segments meet transversally or touch.
    * Collinear overlap returns `None`; the planarizer handles that case by
    * splitting at endpoints instead.
    */
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

/** The sheet of paper. Vertices in counter-clockwise order. */
final case class Polygon(vertices: Vector[Pt]):
  require(vertices.length >= 3, "a sheet of paper needs at least three corners")

  def edges: Vector[Seg] =
    vertices.indices.toVector.map(i => Seg(vertices(i), vertices((i + 1) % vertices.length)))

  def signedArea: Double =
    vertices.indices.map { i =>
      val (p, q) = (vertices(i), vertices((i + 1) % vertices.length))
      p.x * q.y - q.x * p.y
    }.sum / 2

  def area: Double = abs(signedArea)

  def ccw: Polygon = if signedArea < 0 then Polygon(vertices.reverse) else this

  def bounds: (Pt, Pt) =
    (Pt(vertices.map(_.x).min, vertices.map(_.y).min),
     Pt(vertices.map(_.x).max, vertices.map(_.y).max))

  def onBoundary(p: Pt)(using Tol): Boolean = edges.exists(_.contains(p))

  /** Area centroid -- the point the polygon would balance on. */
  def centroid: Pt =
    val n = vertices.length
    var (cx, cy, a) = (0.0, 0.0, 0.0)
    for i <- 0 until n do
      val (p, q) = (vertices(i), vertices((i + 1) % n))
      val cross = p.x * q.y - q.x * p.y
      a += cross
      cx += (p.x + q.x) * cross
      cy += (p.y + q.y) * cross
    if math.abs(a) < 1e-12 then Pt(vertices.map(_.x).sum / n, vertices.map(_.y).sum / n)
    else Pt(cx / (3 * a), cy / (3 * a))

  /** Inside or on the boundary. */
  def contains(p: Pt)(using tol: Tol): Boolean =
    onBoundary(p) || {
      var inside = false
      val n = vertices.length
      var i = 0
      var j = n - 1
      while i < n do
        val (pi, pj) = (vertices(i), vertices(j))
        if (pi.y > p.y) != (pj.y > p.y) &&
          p.x < (pj.x - pi.x) * (p.y - pi.y) / (pj.y - pi.y) + pi.x
        then inside = !inside
        j = i
        i += 1
      inside
    }

  /** Cut an infinite fold line down to the pieces that lie on the paper.
    *
    * A fold is a line in the plane; a *crease* is what survives contact with
    * the sheet. Non-convex sheets can yield several pieces, hence the Vector.
    */
  def clip(l: Line)(using tol: Tol): Vector[Seg] =
    val ts = (edges.flatMap(e => l.intersect(e.line).filter(e.contains)).map(l.paramOf) ++
      vertices.filter(l.contains).map(l.paramOf)).sorted
    val uniq = ts.foldLeft(Vector.empty[Double]): (acc, t) =>
      if acc.lastOption.exists(u => tol.eqv(u, t)) then acc else acc :+ t
    uniq
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

object Geometry:
  /** Angles live on a circle; normalising once, here, keeps everything else honest. */
  def norm2Pi(a: Double): Double =
    val r = a % (2 * Pi)
    if r < 0 then r + 2 * Pi else r

  def degrees(d: Double): Double = d * Pi / 180
