package origami.studio

import origami.geometry.{Geometry, Pt, Tol}
import origami.pattern.CreasePattern
import origami.tiling.{Graded, ShrinkRotate, Tiling, Tilings}

import scala.math.sqrt

/** What the studio's controls describe. Lengths are millimetres; the paper is `paperMm` across (corner to corner
  * for a hexagon, side to side for a square).
  */
final case class Design(kind: String = "trihexagonal", size: Int = 2, depth: Int = 2, focusX: Double = 0.1,
    focusY: Double = 0.05, s: Double = 0.5, twist: Double = 30, paperMm: Double = 200, minPleatMm: Double = 2):

  def alpha: Double = Geometry.degrees(twist)

  /** The tiling in millimetres, and how many refinement points were dropped for narrow pleats. */
  def tiling(using Tol): (Tiling, Int) =
    val r = paperMm / 2
    kind match
      case "hexagonal"  => (Tilings.hexagonal(size, r / (size * sqrt(3))), 0)
      case "square"     => (Tilings.square(size, r / size), 0)
      case "triangular" => (Tilings.triangular(size, r / size), 0)
      case "voronoi" =>
        val params = Graded.Params(paperRadius = r, spacing = r / size, focus = Pt(focusX * r, focusY * r),
          focusRadius = 0.6 * r, depth = depth)
        val pruned = Graded.pruned(params, s, alpha, minPleatMm)
        (pruned.tiling, pruned.dropped.length)
      case _ => (Tilings.trihexagonal(size, r / (2 * size)), 0)

  def pattern(t: Tiling)(using Tol): Either[ShrinkRotate.Problem, CreasePattern] = ShrinkRotate(t, s, alpha)

  /** The pattern even when the parameters are rejected, to show where they go wrong. */
  def preview(t: Tiling)(using Tol): CreasePattern = ShrinkRotate.unchecked(t, s, alpha)

  /** Pleats too narrow to fold by hand, as the points to mark. */
  def narrowPleats(t: Tiling): Vector[Pt] =
    ShrinkRotate.narrowPleats(t, s, alpha, minPleatMm).map: e =>
      val corners = ShrinkRotate.pleat(t, e, s, alpha)
      Pt(corners.map(_.x).sum / 4, corners.map(_.y).sum / 4)

object Design:
  val kinds: Vector[String] = Vector("trihexagonal", "hexagonal", "square", "triangular", "voronoi")

  /** Reads the query string; anything missing or unreadable keeps its default, sizes are kept small. */
  def parse(q: Map[String, String]): Design =
    val d = Design()
    def num(k: String, default: Double) = q.get(k).flatMap(_.toDoubleOption).filterNot(_.isNaN).getOrElse(default)
    def int(k: String, default: Int, max: Int) = q.get(k).flatMap(_.toIntOption).getOrElse(default).max(1).min(max)
    Design(
      kind = q.get("kind").filter(kinds.contains).getOrElse(d.kind),
      size = int("size", d.size, 10),
      depth = q.get("depth").flatMap(_.toIntOption).getOrElse(d.depth).max(0).min(4),
      focusX = num("fx", d.focusX).max(-1).min(1),
      focusY = num("fy", d.focusY).max(-1).min(1),
      s = num("s", d.s),
      twist = num("twist", d.twist),
      paperMm = num("paper", d.paperMm).max(20).min(2000),
      minPleatMm = num("minPleat", d.minPleatMm).max(0))
