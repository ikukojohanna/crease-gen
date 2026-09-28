package origami.laws

import origami.geometry.{Pt, Tol, Vec}
import origami.pattern.{Assignment, PlanarGraph}

/** For each crease, the nearest creases on the parallel lines on either side of it. */
private[laws] object ParallelNeighbours:

  def apply(g: PlanarGraph)(using tol: Tol): Vector[Vector[Int]] =
    def direction(e: Int): Vec = (g.vertices(g.edges(e).v) - g.vertices(g.edges(e).u)).normalized
    def lineOffset(e: Int): Double = direction(e).cross(g.vertices(g.edges(e).u) - Pt.origin)

    val neighbours = Array.fill(g.edges.length)(Vector.empty[Int])
    val creases = g.edges.indices.filterNot(e => g.assignment(e) == Assignment.Boundary)
    for family <- creases.groupBy(e => halfTurnAngle(direction(e))).values do
      val sorted = family.sortBy(lineOffset)
      for (e, i) <- sorted.zipWithIndex do
        def onAnotherLine(k: Int): Boolean = !tol.eqv(lineOffset(sorted(k)), lineOffset(e))
        val before = (i - 1 to 0 by -1).find(onAnotherLine).map(sorted)
        val after = (i + 1 until sorted.length).find(onAnotherLine).map(sorted)
        neighbours(e) = before.toVector ++ after.toVector
    neighbours.toVector

  /** The direction of a line, which does not care which way along it you face. */
  private def halfTurnAngle(d: Vec): Long =
    val a = d.angle
    math.round((if a >= math.Pi then a - math.Pi else a) / 1e-4)
