package origami.geometry

final case class Tri(a: Pt, b: Pt, c: Pt):
  def area: Double = math.abs((b - a).cross(c - a)) / 2
  def vertices: Vector[Pt] = Vector(a, b, c)
  def edges: Vector[(Pt, Pt)] = Vector((a, b), (b, c), (c, a))

  /** Assumes counter-clockwise; points on an edge are outside. */
  def strictlyContains(p: Pt, eps: Double): Boolean =
    edges.forall((u, v) => (p - u).cross(v - u) < -eps * (v - u).norm)
