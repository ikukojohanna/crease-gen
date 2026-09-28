package origami.geometry

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
