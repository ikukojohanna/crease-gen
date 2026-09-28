package origami.output

import origami.folding.FoldedModel
import origami.geometry.Tol
import origami.pattern.Assignment

object FoldedSvg:

  def render(model: FoldedModel, opts: FoldedOptions = FoldedOptions())(using Tol): String =
    val facets = model.foldedFacets
    val bounds = model.state.bounds
    val span = math.max(bounds.width, bounds.height)
    val scale = if span <= 0 then 1.0 else opts.size / span
    val titleH = if opts.title.isDefined then SvgWriter.titleHeight else 0.0
    val w = bounds.width * scale + 2 * opts.margin
    val h = bounds.height * scale + 2 * opts.margin + titleH
    val fullW = math.max(w, opts.title.fold(0.0)(SvgWriter.titleWidth(_, opts.margin)))
    val px = SvgWriter.Projection(bounds, scale, (fullW - bounds.width * scale) / 2, opts.margin, titleH, digits = 2)

    // Bottom first, so upper layers paint over lower ones.
    val body = model.bottomUp.map { f =>
      val fill = if model.state.maps(f).facesUp then opts.front else opts.back
      s"""  <polygon points="${px.points(facets(f).vertices)}" fill="$fill" stroke="${opts.facetEdge}" stroke-width="0.6"/>"""
    }.mkString("\n")

    def foldedEdges(keep: Assignment => Boolean, attrs: String): String =
      model.graph.edges.indices
        .filter(e => keep(model.graph.edges(e).assignment))
        .flatMap(model.state.foldedEdge)
        .map(px.line(_, attrs))
        .mkString("\n")

    val creases = foldedEdges(_.isFolded, s"""stroke="${opts.crease}" stroke-width="0.8"""")

    val rim = foldedEdges(_ == Assignment.Boundary,
      s"""stroke="${opts.paperEdge}" stroke-width="1.4" stroke-linecap="round"""")

    SvgWriter.document(fullW, h, opts.background, Vector(
      opts.title.fold("")(SvgWriter.title(_, opts.margin, baseline = 21)),
      body,
      creases,
      rim
    ))
