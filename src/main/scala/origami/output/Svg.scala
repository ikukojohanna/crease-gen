package origami.output

import origami.geometry.{Seg, Tol}
import origami.laws.FlatFoldable
import origami.output.SvgWriter.Projection
import origami.pattern.{Assignment, CreasePattern}

/** Draws a crease pattern: mountains solid red, valleys dashed blue. */
object Svg:

  private val LegendHeight = 34.0

  def render(cp: CreasePattern, opts: SvgOptions = SvgOptions())(using Tol): String =
    val bounds = cp.paper.bounds
    val (w, h) = (bounds.width * opts.scale, bounds.height * opts.scale)
    val titleH = if opts.title.isDefined then SvgWriter.titleHeight else 0.0
    val legendH = if opts.showLegend then LegendHeight else 0.0
    val fullW = math.max(w + 2 * opts.margin, opts.title.fold(0.0)(SvgWriter.titleWidth(_, opts.margin)))
    val fullH = h + 2 * opts.margin + legendH + titleH
    val px = Projection(bounds, opts.scale, (fullW - w) / 2, opts.margin, titleH, digits = 3)

    SvgWriter.document(fullW, fullH, opts.background, Vector(
      opts.title.fold("")(SvgWriter.title(_, opts.margin, baseline = 20)),
      s"""  <polygon points="${px.points(cp.paper.vertices)}" fill="#ffffff" stroke="none"/>""",
      creases(cp, px, opts),
      cp.paper.edges.map(line(px, _, opts.boundary)).mkString("\n"),
      if opts.showVertices then vertexDots(cp, px) else "",
      if opts.showLegend then legend(opts, y = fullH - opts.margin / 2 - 8) else ""
    ))

  def render(f: FlatFoldable, opts: SvgOptions)(using Tol): String =
    render(f.pattern, opts)

  /** Flat and unassigned creases first, so the folds draw on top of them. */
  private def creases(cp: CreasePattern, px: Projection, opts: SvgOptions): String =
    val (faint, folds) = cp.creases.partition(c => c.assignment == Assignment.Flat || c.assignment == Assignment.Unassigned)
    (faint ++ folds).map(c => line(px, c.seg, opts.styleFor(c.assignment))).mkString("\n")

  private def line(px: Projection, s: Seg, style: Style): String =
    px.line(s, s"""${style.attrs} stroke-linecap="round"""")

  private def vertexDots(cp: CreasePattern, px: Projection)(using Tol): String =
    cp.planarize.vertices.map(px(_)).map((x, y) => f"""  <circle cx="$x%.2f" cy="$y%.2f" r="2" fill="#34495e"/>""")
      .mkString("\n")

  private def legend(opts: SvgOptions, y: Double): String =
    val items = Vector(("mountain", opts.mountain), ("valley", opts.valley), ("edge", opts.boundary))
    items.zipWithIndex.map { case ((label, style), i) =>
      val x = opts.margin + i * 110
      f"""  <line x1="$x%.1f" y1="$y%.1f" x2="${x + 26}%.1f" y2="$y%.1f" ${style.attrs}/>
  <text x="${x + 33}%.1f" y="${y + 4}%.1f" font-family="Helvetica, sans-serif" font-size="11" fill="#555">$label</text>"""
    }.mkString("\n")
