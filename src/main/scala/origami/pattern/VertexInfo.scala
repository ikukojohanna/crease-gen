package origami.pattern

import origami.geometry.{Geometry, Pt, Tol}
import origami.utils.SeqUtils.*

import scala.math.Pi

final case class VertexInfo(
    index: Int,
    at: Pt,
    kind: VertexKind,
    /** Folded or undecided creases, sorted by direction. */
    foldedEdges: Vector[Int],
    directions: Vector[Double]
):
  def degree: Int = foldedEdges.length

  def sectors: Vector[Double] =
    if degree == 0 then Vector(2 * Pi)
    else directions.cyclicPairs.map((from, to) => Geometry.norm2Pi(to - from))

  def littleSectors(using tol: Tol): Vector[VertexInfo.LittleSector] =
    if degree < 4 then Vector.empty
    else
      sectors.cyclicTriples.zip(foldedEdges.cyclicPairs).collect:
        case ((prev, here, next), (left, right)) if tol.lt(here, prev) && tol.lt(here, next) =>
          VertexInfo.LittleSector(here, left, right)

object VertexInfo:
  /** A sector smaller than both neighbours, between creases `left` and `right`. */
  final case class LittleSector(angle: Double, left: Int, right: Int)
