package origami.pattern

import origami.geometry.{Polygon, Pt, Tol}

/** Finds the facets of a crease graph by walking round each face. */
object Faces:

  def apply(g: PlanarGraph)(using Tol): FaceGraph =
    val darts = Darts(g)
    val cycles = darts.faceCycles
    // A slit is walked on both sides, so a vertex may repeat.
    val outlines = cycles.map(_.map(darts.from))
    val bounded = cycles.indices.filter(c => isBounded(outlines(c).map(g.vertices)))

    val facets = bounded.zipWithIndex.map((c, i) => Facet(i, outlines(c), Polygon(outlines(c).map(g.vertices))))
    val facetOfCycle = bounded.zipWithIndex.toMap
    val facetOfDart = Array.fill(darts.count)(-1)
    for (cycle, c) <- cycles.zipWithIndex; d <- cycle do facetOfDart(d) = facetOfCycle.getOrElse(c, -1)

    FaceGraph(g, facets.toVector, facetOfDart.toVector)

  /** Bounded faces run counter-clockwise; the outer face comes out with negative area. */
  private def isBounded(corners: Vector[Pt])(using tol: Tol): Boolean =
    corners.length >= 3 && Polygon(corners).signedArea > tol.value
