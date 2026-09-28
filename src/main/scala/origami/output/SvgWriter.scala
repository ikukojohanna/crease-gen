package origami.output

import origami.geometry.{Bounds, Pt, Seg}

private[output] object SvgWriter:

  /** Paper to SVG coordinates; SVG's y axis points down. */
  final case class Projection(bounds: Bounds, scale: Double, left: Double, margin: Double, titleHeight: Double,
      digits: Int):
    def apply(p: Pt): (Double, Double) =
      ((p.x - bounds.lo.x) * scale + left, (bounds.hi.y - p.y) * scale + margin + titleHeight)

    def points(ps: Seq[Pt]): String =
      ps.map(apply).map((x, y) => s"${fmt(x)},${fmt(y)}").mkString(" ")

    def line(s: Seg, attrs: String): String =
      val (x1, y1) = apply(s.a)
      val (x2, y2) = apply(s.b)
      s"""  <line x1="${fmt(x1)}" y1="${fmt(y1)}" x2="${fmt(x2)}" y2="${fmt(y2)}" $attrs/>"""

    private def fmt(x: Double): String = s"%.${digits}f".format(x)

  def document(width: Double, height: Double, background: String, parts: Seq[String]): String =
    f"""<svg xmlns="http://www.w3.org/2000/svg" width="$width%.1f" height="$height%.1f" viewBox="0 0 $width%.1f $height%.1f">
  <rect width="100%%" height="100%%" fill="$background"/>
${parts.mkString("\n")}
</svg>
"""

  val titleHeight: Double = 30.0

  def titleWidth(title: String, margin: Double): Double = title.length * 8.2 + 2 * margin

  def title(text: String, margin: Double, baseline: Int): String =
    f"""  <text x="$margin%.1f" y="$baseline" font-family="Georgia, serif" font-size="15" fill="#2c3e50">${escape(text)}</text>"""

  def escape(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
