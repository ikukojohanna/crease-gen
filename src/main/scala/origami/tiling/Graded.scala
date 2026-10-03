package origami.tiling

import origami.geometry.{Polygon, Pt, Tol, Vec}

import scala.math.{Pi, sqrt}

/** Voronoi tilings that grow denser towards a focal point.
  *
  * Level 0 is a triangular lattice over the paper; each level adds the lattice of half the spacing inside a disc
  * round the focus, and shrinks the disc. The finer lattice contains the coarser one, so a level only ever adds
  * points. The sites' Voronoi cells, with the sites as centres, are a spider web whatever the points are.
  */
object Graded:

  /** The points so far with the level that added each, the current spacing and the disc still to refine. */
  final case class Level(sites: Vector[(Pt, Int)], spacing: Double, radius: Double, depth: Int)

  final case class Params(paperRadius: Double = 150, spacing: Double = 30, focus: Pt = Pt(20, 10),
      focusRadius: Double = 75, shrink: Double = 0.55, depth: Int = 2)

  /** One step of the recursion: a pure function from a level to the next. */
  def refine(focus: Pt, shrink: Double)(l: Level): Level =
    val h = l.spacing / 2
    val existing = l.sites.map((p, _) => key(p, h)).toSet
    val added = lattice(h, focus, l.radius).filterNot(p => existing(key(p, h)))
    Level(l.sites ++ added.map(_ -> (l.depth + 1)), h, l.radius * shrink, l.depth + 1)

  def sites(p: Params): Vector[(Pt, Int)] =
    val base = Level(lattice(p.spacing, Pt.origin, reach(p)).map(_ -> 0), p.spacing, p.focusRadius, 0)
    Iterator.iterate(base)(refine(p.focus, p.shrink)).drop(p.depth).next().sites

  /** The paper: a regular hexagon, corners at `paperRadius`. */
  def paper(p: Params): Polygon =
    Polygon(Vector.tabulate(6)(k => Pt.origin + Vec.polar(p.paperRadius, Pi * k / 3)))

  /** Cells of the sites well inside the generated points; those near its rim have distorted neighbours. */
  def tiling(p: Params, sites: Vector[Pt])(using Tol): Tiling =
    Voronoi.tiling(sites, paper(p), _.distTo(Pt.origin) <= reach(p) - 2 * p.spacing)

  /** The tiling left after pruning, and the refinement points pruning dropped. */
  final case class Pruned(tiling: Tiling, dropped: Vector[Pt])

  /** Drops refinement points (never level 0) until every pleat is at least `minWidth` wide and long, and every
    * corner admits the pleat angle. Each round drops the finest point at each offending pleat or vertex.
    */
  def pruned(p: Params, s: Double, alpha: Double, minWidth: Double)(using Tol): Pruned =
    val gamma = ShrinkRotate.pleatAngle(s, alpha)
    def go(sites: Vector[(Pt, Int)], dropped: Vector[Pt]): Pruned =
      val t = tiling(p, sites.map(_._1))
      val level = sites.toMap
      val narrow = ShrinkRotate.narrowPleats(t, s, alpha, minWidth).map(e => Vector(e.left, e.right))
      val tight = t.interiorCorners.filter(c => ShrinkRotate.cornerTooTight(c.angle, gamma)).map(c => t.tilesAt(c.point))
      val culprits = (narrow ++ tight).flatMap(tiles => tiles.map(t.tiles(_).centre).filter(level(_) > 0).maxByOption(level))
        .distinct
      if culprits.isEmpty then Pruned(t, dropped)
      else go(sites.filterNot((q, _) => culprits.contains(q)), dropped ++ culprits)
    go(sites(p), Vector.empty)

  /** Points generated this far out, so cells reaching the paper are complete. */
  private def reach(p: Params): Double = p.paperRadius + 4 * p.spacing

  /** A triangular lattice of spacing `h` through the origin, inside a disc. */
  private def lattice(h: Double, centre: Pt, radius: Double): Vector[Pt] =
    val (a1, a2) = (Vec(h, 0), Vec(h / 2, sqrt(3) * h / 2))
    val n = (radius / h * 2).toInt + 2
    val (ci, cj) = ((centre.x - centre.y / sqrt(3)) / h, centre.y / (sqrt(3) * h / 2))
    (for
      i <- (ci - n).toInt to (ci + n).toInt
      j <- (cj - n).toInt to (cj + n).toInt
      q = Pt.origin + a1 * i + a2 * j
      if q.distTo(centre) <= radius
    yield q).toVector

  private def key(p: Pt, h: Double): (Long, Long) = (math.round(p.x / h * 4), math.round(p.y / h * 4))
