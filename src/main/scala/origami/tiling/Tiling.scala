package origami.tiling

import origami.geometry.{Geometry, Polygon, Pt, Tol}
import origami.pattern.{Edge, PointIndex}
import origami.utils.SeqUtils.*

/** A tile: its corners counter-clockwise, as indices into the tiling's points, and the point it rotates about. */
final case class Tile(corners: Vector[Int], centre: Pt)

/** The angle inside tile `tile` at point `point`. */
final case class Corner(tile: Int, point: Int, angle: Double)

/** An edge two tiles share; it runs `u` to `v` counter-clockwise round `left`, so `right` lies on its right. */
final case class SharedEdge(u: Int, v: Int, left: Int, right: Int)

/** Edge-to-edge polygons, each with a rotation centre, cut out by a sheet of paper. Only `Tiling.of` builds one,
  * so corners are always shared points and always counter-clockwise.
  */
final case class Tiling private (points: Vector[Pt], tiles: Vector[Tile], paper: Polygon):

  def polygon(t: Int): Polygon = Polygon(tiles(t).corners.map(points))

  /** Every edge with a tile on each side. */
  lazy val sharedEdges: Vector[SharedEdge] =
    val sides = for (tile, t) <- tiles.zipWithIndex; (u, v) <- tile.corners.cyclicPairs yield (Edge.key(u, v), (u, v, t))
    sides.groupMap(_._1)(_._2).values.toVector.collect:
      case Vector((u, v, a), (_, _, b)) => SharedEdge(u, v, a, b)

  /** The tiles round each point. */
  lazy val tilesAt: Vector[Vector[Int]] =
    val at = for (tile, t) <- tiles.zipWithIndex; c <- tile.corners yield (c, t)
    val grouped = at.groupMap(_._1)(_._2)
    points.indices.toVector.map(grouped.getOrElse(_, Vector.empty))

  /** A point with tiles all the way round it. */
  def isInterior(p: Int): Boolean =
    val around = tilesAt(p)
    val edgesAround = for t <- around; (u, v) <- tiles(t).corners.cyclicPairs if u == p || v == p yield Edge.key(u, v)
    around.nonEmpty && edgesAround.groupBy(identity).values.forall(_.length == 2)

  /** The same tiling seen in a mirror (y negated). */
  def mirrored(using Tol): Tiling =
    def flip(poly: Polygon) = Polygon(poly.vertices.map(Tiling.mirror))
    Tiling.of(tiles.indices.toVector.map(i => (flip(polygon(i)), Tiling.mirror(tiles(i).centre))), flip(paper))

  /** Every tile corner at an interior point, the corners twists form round. */
  def interiorCorners: Vector[Corner] =
    for
      (tile, t) <- tiles.zipWithIndex
      (prev, here, next) <- tile.corners.cyclicTriples
      if isInterior(here)
    yield
      val (a, b) = (points(next) - points(here), points(prev) - points(here))
      Corner(t, here, Geometry.norm2Pi(math.atan2(a.cross(b), a.dot(b))))

object Tiling:
  /** The mirror `mirrored` uses. */
  def mirror(p: Pt): Pt = Pt(p.x, -p.y)

  /** Tiles as polygons with their centres; corners closer than the tolerance become one point. */
  def of(tiles: Vector[(Polygon, Pt)], paper: Polygon)(using tol: Tol): Tiling =
    val index = PointIndex(tol)
    val indexed = tiles.map((poly, centre) => Tile(withoutRepeats(poly.ccw.vertices.map(index.indexOf)), centre))
    Tiling(index.all, indexed, paper.ccw)

  /** Corners that merged into one point count once. */
  private def withoutRepeats(corners: Vector[Int]): Vector[Int] =
    corners.distinctConsecutiveWith(_ == _) match
      case cs if cs.length > 1 && cs.head == cs.last => cs.init
      case cs                                        => cs
