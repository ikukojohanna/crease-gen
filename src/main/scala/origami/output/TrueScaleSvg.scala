package origami.output

import origami.geometry.{Pt, Seg}
import origami.pattern.{Assignment, CreasePattern}

/** A crease pattern at true size: one unit is one millimetre, for printing or a cutting plotter. */
object TrueScaleSvg:

  private val Line = 0.25

  def render(cp: CreasePattern, opts: SvgOptions = SvgOptions()): String =
    val b = cp.paper.bounds
    def xy(p: Pt): (Double, Double) = (p.x - b.lo.x, b.hi.y - p.y)
    def at(p: Pt): String = xy(p) match { case (x, y) => f"$x%.3f,$y%.3f" }
    def line(s: Seg, style: Style, dash: Option[String]): String =
      val ((x1, y1), (x2, y2)) = (xy(s.a), xy(s.b))
      val dashes = dash.fold("")(d => s""" stroke-dasharray="$d"""")
      f"""  <line x1="$x1%.3f" y1="$y1%.3f" x2="$x2%.3f" y2="$y2%.3f" stroke="${style.stroke}" stroke-width="$Line"$dashes/>"""
    def dashFor(a: Assignment): Option[String] = a match
      case Assignment.Valley => Some("2 1")
      case Assignment.Flat | Assignment.Unassigned | Assignment.Optional => Some("0.4 1")
      case _ => None

    val creases = cp.creases.map(c => line(c.seg, opts.styleFor(c.assignment), dashFor(c.assignment)))
    val outline = cp.paper.vertices.map(at).mkString(" ")
    f"""<svg xmlns="http://www.w3.org/2000/svg" width="${b.width}%.3fmm" height="${b.height}%.3fmm" viewBox="0 0 ${b.width}%.3f ${b.height}%.3f">
  <polygon points="$outline" fill="none" stroke="${opts.boundary.stroke}" stroke-width="$Line"/>
${creases.mkString("\n")}
</svg>
"""
