package origami

/** How each kind of crease is drawn. The convention is the standard one from
  * origami diagrams: mountains solid red, valleys dashed blue. */
final case class Style(stroke: String, width: Double, dash: Option[String]):
  def attrs: String =
    s"""stroke="$stroke" stroke-width="$width"""" + dash.fold("")(d => s""" stroke-dasharray="$d"""")

final case class SvgOptions(
    scale: Double = 1.0,
    margin: Double = 24,
    background: String = "#fffdf7",
    title: Option[String] = None,
    showVertices: Boolean = false,
    showLegend: Boolean = true,
    mountain: Style = Style("#c0392b", 1.6, None),
    valley: Style = Style("#2471a3", 1.6, Some("7 4")),
    boundary: Style = Style("#2c3e50", 2.2, None),
    flat: Style = Style("#b8b8b0", 1.0, Some("2 4")),
    unassigned: Style = Style("#7f8c8d", 1.4, Some("1 3"))
):
  def styleFor(a: Assignment): Style = a match
    case Assignment.Mountain   => mountain
    case Assignment.Valley     => valley
    case Assignment.Boundary   => boundary
    case Assignment.Flat       => flat
    case Assignment.Unassigned => unassigned

/** Rendering is the only part of this program that is allowed to be
  * approximate: it turns exact geometry into pixels. Everything upstream of it
  * stays in the plane.
  */
object Svg:

  def render(cp: CreasePattern, opts: SvgOptions = SvgOptions())(using Tol): String =
    val (lo, hi) = cp.paper.bounds
    val w = (hi.x - lo.x) * opts.scale
    val h = (hi.y - lo.y) * opts.scale
    val legendH = if opts.showLegend then 34.0 else 0.0
    val titleH = if opts.title.isDefined then 30.0 else 0.0
    // A long title must not run off the sheet.
    val titleW = opts.title.fold(0.0)(_.length * 8.2 + 2 * opts.margin)
    val fullW = math.max(w + 2 * opts.margin, titleW)
    val fullH = h + 2 * opts.margin + legendH + titleH

    // SVG's y axis points down; the paper's points up.
    val xOffset = (fullW - w) / 2
    def px(p: Pt): (Double, Double) =
      ((p.x - lo.x) * opts.scale + xOffset,
       (hi.y - p.y) * opts.scale + opts.margin + titleH)

    def line(s: Seg, st: Style): String =
      val (x1, y1) = px(s.a)
      val (x2, y2) = px(s.b)
      f"""  <line x1="$x1%.3f" y1="$y1%.3f" x2="$x2%.3f" y2="$y2%.3f" ${st.attrs} stroke-linecap="round"/>"""

    val paperPath =
      cp.paper.vertices.map(px).map((x, y) => f"$x%.3f,$y%.3f").mkString(" ")

    // Draw flat and unassigned creases first so folds sit on top of them.
    val ordered = cp.creases.sortBy(c => c.assignment match
      case Assignment.Flat | Assignment.Unassigned => 0
      case _ => 1)

    val body = ordered.map(c => line(c.seg, opts.styleFor(c.assignment))).mkString("\n")
    val boundary = cp.paper.edges.map(e => line(e, opts.boundary)).mkString("\n")

    val vertices =
      if !opts.showVertices then ""
      else cp.planarize.vertices.map(px).map((x, y) =>
        f"""  <circle cx="$x%.2f" cy="$y%.2f" r="2" fill="#34495e"/>""").mkString("\n")

    val titleEl = opts.title.fold(""): t =>
      f"""  <text x="${opts.margin}%.1f" y="20" font-family="Georgia, serif" font-size="15" fill="#2c3e50">${esc(t)}</text>"""

    val legend =
      if !opts.showLegend then ""
      else
        val y = fullH - opts.margin / 2 - 8
        val items = Vector(
          ("mountain", opts.mountain), ("valley", opts.valley), ("edge", opts.boundary))
        items.zipWithIndex.map { case ((label, st), i) =>
          val x = opts.margin + i * 110
          f"""  <line x1="$x%.1f" y1="$y%.1f" x2="${x + 26}%.1f" y2="$y%.1f" ${st.attrs}/>
  <text x="${x + 33}%.1f" y="${y + 4}%.1f" font-family="Helvetica, sans-serif" font-size="11" fill="#555">$label</text>"""
        }.mkString("\n")

    f"""<svg xmlns="http://www.w3.org/2000/svg" width="$fullW%.1f" height="$fullH%.1f" viewBox="0 0 $fullW%.1f $fullH%.1f">
  <rect width="100%%" height="100%%" fill="${opts.background}"/>
$titleEl
  <polygon points="$paperPath" fill="#ffffff" stroke="none"/>
$body
$boundary
$vertices
$legend
</svg>
"""

  def render(f: FlatFoldable, opts: SvgOptions)(using Tol): String =
    render(f.pattern, opts)

  private def esc(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

final case class FoldedOptions(
    size: Double = 340,
    margin: Double = 28,
    background: String = "#fffdf7",
    title: Option[String] = None,
    /** The side of the paper that started face-up, and the other one. */
    front: String = "#fdf3e3",
    back: String = "#e8d9c0",
    facetEdge: String = "#00000022",
    paperEdge: String = "#2c3e50",
    crease: String = "#0000001a"
)

/** Draw the folded model: the facets in stacking order, bottom first.
  *
  * This is the picture the crease pattern is a plan for. Layers that lie on top
  * of other layers hide them, exactly as paper does, so the drawing is made the
  * way the object is made -- which is only possible because the program worked
  * out the order.
  */
object FoldedSvg:

  def render(model: FoldedModel, opts: FoldedOptions = FoldedOptions())(using Tol): String =
    val facets = model.foldedFacets
    val (lo, hi) = model.state.bounds
    val span = math.max(hi.x - lo.x, hi.y - lo.y)
    val scale = if span <= 0 then 1.0 else opts.size / span
    val titleH = if opts.title.isDefined then 30.0 else 0.0
    val w = (hi.x - lo.x) * scale + 2 * opts.margin
    val h = (hi.y - lo.y) * scale + 2 * opts.margin + titleH
    val fullW = math.max(w, opts.title.fold(0.0)(_.length * 8.2 + 2 * opts.margin))
    val xOffset = (fullW - (hi.x - lo.x) * scale) / 2

    def px(p: Pt): (Double, Double) =
      ((p.x - lo.x) * scale + xOffset, (hi.y - p.y) * scale + opts.margin + titleH)

    def path(poly: Polygon): String =
      poly.vertices.map(px).map((x, y) => f"$x%.2f,$y%.2f").mkString(" ")

    // Bottom of the stack first: later facets paint over earlier ones, which is
    // what "on top" means.
    val body = model.bottomUp.map { f =>
      val fill = if model.state.maps(f).facesUp then opts.front else opts.back
      f"""  <polygon points="${path(facets(f))}" fill="$fill" stroke="${opts.facetEdge}" stroke-width="0.6"/>"""
    }.mkString("\n")

    val creases = model.graph.edges.indices.flatMap { e =>
      Option.when(model.graph.edges(e).assignment.isFolded):
        model.state.foldedEdge(e).map: s =>
          val (x1, y1) = px(s.a)
          val (x2, y2) = px(s.b)
          f"""  <line x1="$x1%.2f" y1="$y1%.2f" x2="$x2%.2f" y2="$y2%.2f" stroke="${opts.crease}" stroke-width="0.8"/>"""
    }.flatten.mkString("\n")

    // The raw edges of the sheet: in a folded model these are the cut edges,
    // and they are what makes the shape readable.
    val rim = model.graph.edges.indices.flatMap { e =>
      Option.when(model.graph.edges(e).assignment == Assignment.Boundary):
        model.state.foldedEdge(e).map: s =>
          val (x1, y1) = px(s.a)
          val (x2, y2) = px(s.b)
          f"""  <line x1="$x1%.2f" y1="$y1%.2f" x2="$x2%.2f" y2="$y2%.2f" stroke="${opts.paperEdge}" stroke-width="1.4" stroke-linecap="round"/>"""
    }.flatten.mkString("\n")

    val titleEl = opts.title.fold(""): t =>
      f"""  <text x="${opts.margin}%.1f" y="21" font-family="Georgia, serif" font-size="15" fill="#2c3e50">${t.replace("&", "&amp;").replace("<", "&lt;")}</text>"""

    f"""<svg xmlns="http://www.w3.org/2000/svg" width="$fullW%.1f" height="$h%.1f" viewBox="0 0 $fullW%.1f $h%.1f">
  <rect width="100%%" height="100%%" fill="${opts.background}"/>
$titleEl
$body
$creases
$rim
</svg>
"""
