package origami.tiling

import origami.geometry.{Polygon, Pt, Tol}

/** Voronoi cells with their sites as centres. Every Voronoi edge is the perpendicular bisector of the two sites it
  * separates, so the result is a spider web by construction.
  */
object Voronoi:

  /** The cells of the sites `keep` accepts. Cells of sites on the hull are open and never kept. */
  def tiling(sites: Vector[Pt], paper: Polygon, keep: Pt => Boolean)(using Tol): Tiling =
    val d = Delaunay(sites)
    val around = d.triangles.flatMap(t => t.corners.map(_ -> t)).groupMap(_._1)(_._2)
    val cells =
      for
        (site, triangles) <- around.toVector.sortBy(_._1)
        if !d.hull(site) && keep(sites(site)) && triangles.length >= 3
      yield
        val corners = triangles.map(d.circumcentre).sortBy(c => (c - sites(site)).angle)
        (Polygon(corners), sites(site))
    Tiling.of(cells, paper)
