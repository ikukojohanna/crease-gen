package origami.pattern

import origami.geometry.Seg

final case class FaceGraph(graph: PlanarGraph, facets: Vector[Facet], faceOfDart: Vector[Int]):
  def dartsOf(edge: Int): (Int, Int) = (2 * edge, 2 * edge + 1)

  def facesAt(edge: Int): Vector[Int] =
    Vector(faceOfDart(2 * edge), faceOfDart(2 * edge + 1)).filter(_ >= 0).distinct

  def neighbours(face: Int): Vector[(Int, Int)] =
    graph.edges.indices.toVector.flatMap: e =>
      facesAt(e) match
        case Vector(x, y) if x == face => Some((e, y))
        case Vector(x, y) if y == face => Some((e, x))
        case _                         => None

  def edgeSeg(edge: Int): Seg =
    val e = graph.edges(edge)
    Seg(graph.vertices(e.u), graph.vertices(e.v))
