package origami.tiling

import origami.geometry.{Pt, Tol}

/** The spider-web condition: the segment joining two neighbours' centres crosses their shared edge at right angles,
  * going from the left tile to the right one.
  */
object Reciprocal:

  /** A shared edge that breaks the condition, with the angle between centre segment and edge (90° is right). */
  final case class Violation(at: Pt, angleDegrees: Double):
    def explain: String = f"(${at.x}%.2f, ${at.y}%.2f): centres meet the shared edge at $angleDegrees%.2f deg, not 90"

  def check(t: Tiling)(using tol: Tol): Vector[Violation] =
    t.sharedEdges.flatMap: e =>
      val (a, b) = (t.points(e.u), t.points(e.v))
      val edge = b - a
      val across = t.tiles(e.right).centre - t.tiles(e.left).centre
      val angle = math.toDegrees(math.atan2(edge.cross(across), edge.dot(across)))
      // Measured from the edge, a centre segment pointing from left to right turns clockwise: -90 degrees.
      Option.unless(math.abs(angle + 90) <= 1e-6 && across.norm > tol.value)(Violation(a.midpoint(b), -angle))
