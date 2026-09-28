package origami.geometry

final case class Bounds(lo: Pt, hi: Pt):
  def width: Double = hi.x - lo.x
  def height: Double = hi.y - lo.y

object Bounds:
  def of(ps: Seq[Pt]): Bounds =
    Bounds(Pt(ps.map(_.x).min, ps.map(_.y).min), Pt(ps.map(_.x).max, ps.map(_.y).max))
