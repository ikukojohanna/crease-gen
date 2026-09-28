package origami.geometry

final case class Rigid(a: Double, b: Double, c: Double, d: Double, tx: Double, ty: Double):
  def apply(p: Pt): Pt = Pt(a * p.x + b * p.y + tx, c * p.x + d * p.y + ty)
  def apply(v: Vec): Vec = Vec(a * v.x + b * v.y, c * v.x + d * v.y)
  def apply(s: Seg): Seg = Seg(apply(s.a), apply(s.b))
  def apply(poly: Polygon): Polygon = Polygon(poly.vertices.map(apply))

  def det: Double = a * d - b * c

  /** The side that started face-up is showing. */
  def facesUp: Boolean = det > 0

  /** Apply `first`, then `this`. */
  def compose(first: Rigid): Rigid = Rigid(
    a * first.a + b * first.c, a * first.b + b * first.d,
    c * first.a + d * first.c, c * first.b + d * first.d,
    a * first.tx + b * first.ty + tx, c * first.tx + d * first.ty + ty)

  def andThen(next: Rigid): Rigid = next.compose(this)

  def approx(o: Rigid)(using tol: Tol): Boolean =
    val linear = tol.isZero(a - o.a) && tol.isZero(b - o.b) && tol.isZero(c - o.c) && tol.isZero(d - o.d)
    linear && tol.isZero(tx - o.tx) && tol.isZero(ty - o.ty)

object Rigid:
  val identity: Rigid = Rigid(1, 0, 0, 1, 0, 0)

  def reflection(l: Line): Rigid =
    val (dx, dy) = (l.dir.x, l.dir.y)
    val (a, b, c, d) = (dx * dx - dy * dy, 2 * dx * dy, 2 * dx * dy, dy * dy - dx * dx)
    val o = l.origin
    Rigid(a, b, c, d, o.x - (a * o.x + b * o.y), o.y - (c * o.x + d * o.y))

  def translation(v: Vec): Rigid = Rigid(1, 0, 0, 1, v.x, v.y)
