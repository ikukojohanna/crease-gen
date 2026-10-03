package origami.tiling

import origami.geometry.{Polygon, Pt, Tol, Vec}

import scala.math.{Pi, sqrt}

/** The regular and semi-regular tilings, centres at the tiles' centroids (where the perpendicular bisectors meet).
  *
  * Boundary: the paper is a regular polygon of `size` cells' radius, centred on a tiling vertex. Tiles are generated
  * two cells beyond it, so every crease that reaches the paper comes from a complete twist and is simply cut off
  * at the paper's edge, the way a tessellation is cut from a larger sheet.
  */
object Tilings:

  /** 6.6.6: hexagons, three at every vertex. */
  def hexagonal(size: Int, edge: Double = 20)(using Tol): Tiling =
    val (a1, a2) = (Vec(sqrt(3) * edge, 0), Vec(sqrt(3) * edge / 2, 1.5 * edge))
    val hexagon = regular(6, edge, Pt(0, edge), turn = Pi / 6)
    lattice(a1, a2, Vector(hexagon), hexagonPaper(size * sqrt(3) * edge), reach = size + 2)

  /** 4.4.4.4: squares. */
  def square(size: Int, edge: Double = 20)(using Tol): Tiling =
    val square = Polygon(Vector(Pt(0, 0), Pt(edge, 0), Pt(edge, edge), Pt(0, edge)))
    val half = size * edge
    val paper = Polygon(Vector(Pt(-half, -half), Pt(half, -half), Pt(half, half), Pt(-half, half)))
    lattice(Vec(edge, 0), Vec(0, edge), Vector(square), paper, reach = size + 2)

  /** 3.3.3.3.3.3: triangles, six at every vertex. */
  def triangular(size: Int, edge: Double = 20)(using Tol): Tiling =
    val (a1, a2) = (Vec(edge, 0), Vec(edge / 2, sqrt(3) * edge / 2))
    val up = Polygon(Vector(Pt(0, 0), Pt(edge, 0), Pt(edge / 2, sqrt(3) * edge / 2)))
    val down = Polygon(Vector(Pt(edge, 0), Pt(1.5 * edge, sqrt(3) * edge / 2), Pt(edge / 2, sqrt(3) * edge / 2)))
    lattice(a1, a2, Vector(up, down), hexagonPaper(size * edge), reach = size + 2)

  /** 3.6.3.6: hexagons and triangles alternating round every vertex (the kagome). */
  def trihexagonal(size: Int, edge: Double = 20)(using Tol): Tiling =
    val (a1, a2) = (Vec(2 * edge, 0), Vec(edge, sqrt(3) * edge))
    val hexagon = regular(6, edge, Pt(-edge, 0), turn = 0)
    val h = sqrt(3) * edge / 2
    val up = Polygon(Vector(Pt(0, 0), Pt(edge / 2, h), Pt(-edge / 2, h)))
    val down = Polygon(Vector(Pt(0, 0), Pt(-edge / 2, -h), Pt(edge / 2, -h)))
    lattice(a1, a2, Vector(hexagon, up, down), hexagonPaper(size * 2 * edge), reach = size + 2)

  /** A motif repeated over a lattice, every tile kept whose centre lies within `reach` cells of the origin. */
  private def lattice(a1: Vec, a2: Vec, motif: Vector[Polygon], paper: Polygon, reach: Int)(using Tol): Tiling =
    val radius = paper.vertices.map(_.distTo(Pt.origin)).max + 2 * math.max(a1.norm, a2.norm)
    val n = 2 * reach + 2
    val tiles =
      for
        i <- -n to n
        j <- -n to n
        shape <- motif
        tile = Polygon(shape.vertices.map(_ + a1 * i + a2 * j))
        if tile.centroid.distTo(Pt.origin) <= radius
      yield (tile, tile.centroid)
    Tiling.of(tiles.toVector, paper)

  private def regular(n: Int, radius: Double, centre: Pt, turn: Double): Polygon =
    Polygon(Vector.tabulate(n)(k => centre + Vec.polar(radius, turn + 2 * Pi * k / n)))

  /** A regular hexagon about the origin, corners at `radius`. */
  private def hexagonPaper(radius: Double): Polygon = regular(6, radius, Pt.origin, turn = 0)
