package origami

import scala.collection.mutable

/** Where every facet ends up once the paper is folded flat.
  *
  * Built by walking the facets and composing reflections: start anywhere, call
  * that facet's position the identity, andeach time you cross a folded crease,
  * compose with the reflection in that crease line. That is the whole
  * algorithm, and it is the whole of origami -- a folded model is a product of
  * reflections, indexed by which face you are looking at.
  *
  * The walk raises an obvious question: the facets form loops, so does the
  * answer depend on the route? It does not, and the reason is Kawasaki's
  * theorem. Reflections in lines through a common point compose to a rotation
  * by twice the alternating sum of the angles between them, so the loop around
  * an interior vertex is the identity exactly when that alternating sum
  * vanishes -- which is what Kawasaki says.
  *
  * Kawasaki's theorem is not a rule about crease patterns that we check on the
  * side. It is the statement that this function is well defined.
  */
final case class FoldedState(faces: FaceGraph, maps: Vector[Rigid]):
  def graph: PlanarGraph = faces.graph

  def folded(face: Int): Polygon = maps(face)(faces.facets(face).polygon)
  def foldedFacets: Vector[Polygon] = faces.facets.indices.toVector.map(folded)

  /** The crease as it appears in the folded model. */
  def foldedEdge(edge: Int): Option[Seg] =
    faces.facesAt(edge).headOption.map(f => maps(f)(faces.edgeSeg(edge)))

  /** Facets showing the side of the paper that started face-up. */
  def faceUpCount: Int = maps.count(_.facesUp)

  def bounds: (Pt, Pt) =
    val ps = foldedFacets.flatMap(_.vertices)
    (Pt(ps.map(_.x).min, ps.map(_.y).min), Pt(ps.map(_.x).max, ps.map(_.y).max))

  /** How much smaller the model is than the sheet: the flat-folding ratio. */
  def compression: Double =
    val (lo, hi) = bounds
    graph.paper.area / math.max(1e-9, (hi.x - lo.x) * (hi.y - lo.y))

object FoldedState:

  /** Fold the pattern, or report the creases where the map fails to agree with
    * itself. A pattern that has passed Kawasaki never produces a Left.
    */
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

      // Facets unreachable only if the sheet fell apart; pin them where they are.
      val settled = maps.map(_.getOrElse(Rigid.identity)).toVector

      // Now check every crease, including the ones the walk did not use. This is
      // where an inconsistent pattern gives itself away.
      val bad = g.edges.indices.toVector.flatMap: edge =>
        faces.facesAt(edge) match
          case Vector(x, y) if !across(settled(x), g, faces, edge).approx(settled(y)) =>
            Some(Violation.Inconsistent(faces.edgeSeg(edge).midpoint))
          case _ => None

      if bad.isEmpty then Right(FoldedState(faces, settled)) else Left(bad)

  /** A pattern that carries its proof folds without the possibility of failure. */
  def of(f: FlatFoldable)(using Tol): FoldedState =
    from(f.graph).getOrElse(throw AssertionError("a FlatFoldable folded inconsistently"))

  /** Crossing a crease composes the position so far with a reflection in it --
    * unless the crease was never folded, in which case nothing happens. */
  private def across(here: Rigid, g: PlanarGraph, faces: FaceGraph, edge: Int): Rigid =
    if !g.edges(edge).assignment.isFolded then here
    else here.compose(Rigid.reflection(faces.edgeSeg(edge).line))
