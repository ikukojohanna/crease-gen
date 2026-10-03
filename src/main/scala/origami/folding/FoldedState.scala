package origami.folding

import origami.geometry.{Bounds, Polygon, Rigid, Seg, Tol}
import origami.laws.{FlatFoldable, Violation}
import origami.pattern.{FaceGraph, Faces, PlanarGraph}

import scala.collection.mutable

final case class FoldedState(faces: FaceGraph, maps: Vector[Rigid]):
  def graph: PlanarGraph = faces.graph

  def folded(face: Int): Polygon = maps(face)(faces.facets(face).polygon)
  def foldedFacets: Vector[Polygon] = faces.facets.indices.toVector.map(folded)

  def foldedEdge(edge: Int): Option[Seg] =
    faces.facesAt(edge).headOption.map(f => maps(f)(faces.edgeSeg(edge)))

  def faceUpCount: Int = maps.count(_.facesUp)

  def bounds: Bounds = Bounds.of(foldedFacets.flatMap(_.vertices))

  def compression: Double =
    val b = bounds
    graph.paper.area / math.max(1e-9, b.width * b.height)

object FoldedState:

  /** Left when two routes to a facet disagree, i.e. Kawasaki fails. */
  def from(g: PlanarGraph)(using tol: Tol): Either[Vector[Violation], FoldedState] =
    val faces = Faces(g)
    if faces.facets.isEmpty then Right(FoldedState(faces, Vector.empty))
    else
      val maps = Array.fill(faces.facets.length)(Option.empty[Rigid])
      maps(0) = Some(Rigid.identity)

      val queue = mutable.Queue(0)
      while queue.nonEmpty do
        val f = queue.dequeue()
        val here = maps(f).get
        for (edge, other) <- faces.neighbours(f) if maps(other).isEmpty do
          maps(other) = Some(across(here, g, faces, edge))
          queue.enqueue(other)

      val settled = maps.map(_.getOrElse(Rigid.identity)).toVector

      val bad = g.edges.indices.toVector.flatMap: edge =>
        faces.facesAt(edge) match
          case Vector(x, y) if !across(settled(x), g, faces, edge).approx(settled(y)) =>
            Some(Violation.Inconsistent(faces.edgeSeg(edge).midpoint))
          case _ => None

      if bad.isEmpty then Right(FoldedState(faces, settled)) else Left(bad)

  def of(f: FlatFoldable)(using Tol): FoldedState =
    from(f.graph).getOrElse(throw AssertionError("a FlatFoldable folded inconsistently"))

  private def across(here: Rigid, g: PlanarGraph, faces: FaceGraph, edge: Int): Rigid =
    if !g.edges(edge).assignment.folds then here
    else here.compose(Rigid.reflection(faces.edgeSeg(edge).line))
