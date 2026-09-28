package origami.pattern

import scala.collection.mutable

/** The half-edges of a graph: edge `e` gives dart `2e` from `u` to `v` and dart `2e + 1` back. */
private[pattern] final class Darts(g: PlanarGraph):
  val count: Int = 2 * g.edges.length

  def from(d: Int): Int = if d % 2 == 0 then g.edges(d / 2).u else g.edges(d / 2).v
  def to(d: Int): Int = from(reverse(d))
  def reverse(d: Int): Int = d ^ 1

  /** The darts leaving each vertex, counter-clockwise. */
  private val leaving: Vector[Vector[Int]] =
    val byVertex = (0 until count).groupBy(from)
    g.vertices.indices.toVector.map(v => byVertex.getOrElse(v, Vector.empty).sortBy(angle).toVector)

  private val positionAround: Array[Int] =
    val position = Array.fill(count)(0)
    for around <- leaving; (d, i) <- around.zipWithIndex do position(d) = i
    position

  /** The next dart round the same face: back along the edge, then the next dart clockwise. */
  def next(d: Int): Int =
    val back = reverse(d)
    val around = leaving(from(back))
    around((positionAround(back) - 1 + around.length) % around.length)

  /** Every face as the cycle of darts around it. */
  def faceCycles: Vector[Vector[Int]] =
    val visited = mutable.BitSet.empty
    (0 until count).toVector.flatMap: start =>
      Option.unless(visited(start)):
        val cycle = start +: Iterator.iterate(next(start))(next).takeWhile(_ != start).toVector
        visited ++= cycle
        cycle

  private def angle(d: Int): Double = (g.vertices(to(d)) - g.vertices(from(d))).angle
