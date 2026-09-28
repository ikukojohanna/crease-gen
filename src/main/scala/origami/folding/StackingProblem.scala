package origami.folding

import origami.geometry.{Seg, Tol, Triangulate}
import origami.pattern.Assignment

import scala.collection.mutable

/** "Paper cannot pass through paper", written as constraints on the layer order of one folded state.
  *
  * A relation `(a, b)` means "facet `a` lies above facet `b`". Each constraint is a set of relations
  * of which an even number must hold.
  */
private[folding] final class StackingProblem(state: FoldedState, conditions: Layers.Conditions)(using tol: Tol):
  import StackingProblem.FoldedCrease

  val facetCount: Int = state.faces.facets.length

  private val facets = state.foldedFacets
  private val triangles = facets.map(Triangulate.apply)
  private val eps = math.sqrt(math.max(1e-9, state.graph.paper.area)) * 1e-6

  /** Pairs of facets that share area once folded. Only these need an order. */
  val overlappingPairs: Vector[(Int, Int)] =
    (for
      i <- 0 until facetCount
      j <- (i + 1) until facetCount
      if triangles(i).exists(a => triangles(j).exists(b => Overlap.triangles(a, b, eps)))
    yield (i, j)).toVector

  private val overlapsWith: Array[mutable.BitSet] =
    val sets = Array.fill(facetCount)(mutable.BitSet.empty)
    for (i, j) <- overlappingPairs do
      sets(i) += j
      sets(j) += i
    sets

  private def overlapping(a: Int, b: Int): Boolean = overlapsWith(a)(b)

  private val creases: Vector[FoldedCrease] =
    state.graph.edges.indices.toVector.flatMap: e =>
      val sides = state.faces.facesAt(e)
      val assignment = state.graph.assignment(e)
      Option.when((assignment.isFolded || assignment == Assignment.Flat) && sides.length == 2):
        FoldedCrease(state.maps(sides(0))(state.faces.edgeSeg(e)), sides(0), sides(1), assignment)

  /** At every fold, the relation between its two facets, which the fold itself decides. */
  val forced: Vector[(Int, Int)] = creases.filter(_.folded).map(topAndBottom)

  val constraints: Vector[Vector[(Int, Int)]] =
    (if conditions.tacoTaco then tacoTaco else Vector.empty) ++ creaseThroughFacet

  /** Valley-folding a face-up sheet puts the moving facet on top; a mountain or a face-down sheet swaps that. */
  private def topAndBottom(crease: FoldedCrease): (Int, Int) =
    val valley = crease.assignment == Assignment.Valley
    val rightOnTop = if state.maps(crease.left).facesUp then valley else !valley
    if rightOnTop then (crease.right, crease.left) else (crease.left, crease.right)

  /** Two folds on the same line may nest, but not interleave. */
  private def tacoTaco: Vector[Vector[(Int, Int)]] =
    for
      i <- creases.indices.toVector
      j <- (i + 1) until creases.length
      (t1, t2) = (creases(i), creases(j))
      if t1.folded && t2.folded && onSameLine(t1, t2) && openSameWay(t1, t2)
      constraint <- nestOrMiss(t1, t2)
    yield constraint

  /** A crease through a facet keeps that facet on one side of both of the crease's facets. */
  private def creaseThroughFacet: Vector[Vector[(Int, Int)]] =
    for
      t <- creases
      if (if t.folded then conditions.tacoTortilla else conditions.tortillaTortilla)
      f <- 0 until facetCount
      if f != t.left && f != t.right && triangles(f).exists(Overlap.segmentThrough(t.seg, _, eps))
      constraint <- sameSide(t.left, t.right, f)
    yield constraint

  private def nestOrMiss(t1: FoldedCrease, t2: FoldedCrease): Option[Vector[(Int, Int)]] =
    val (a, b, c, d) = (t1.left, t1.right, t2.left, t2.right)
    val relations = Vector((a, c), (b, c), (a, d), (b, d))
    Option.when(Set(a, b, c, d).size == 4 && relations.forall(overlapping))(relations)

  private def sameSide(a: Int, b: Int, c: Int): Option[Vector[(Int, Int)]] =
    Option.when(a != b && overlapping(a, c) && overlapping(b, c))(Vector((a, c), (b, c)))

  private def onSameLine(t1: FoldedCrease, t2: FoldedCrease): Boolean =
    t1.seg.line.sameAs(t2.seg.line) && t1.seg.collinearOverlap(t2.seg, eps).isDefined

  /** Back to back, two folds on one line never meet. */
  private def openSameWay(t1: FoldedCrease, t2: FoldedCrease): Boolean =
    val l = t1.seg.line
    def side(f: Int): Double = math.signum(l.signedDist(facets(f).centroid))
    side(t1.left) == side(t1.right) && side(t2.left) == side(t2.right) && side(t1.left) == side(t2.left)

private[folding] object StackingProblem:
  /** A crease where it lands in the folded model, with the facets on either side of it. */
  final case class FoldedCrease(seg: Seg, left: Int, right: Int, assignment: Assignment):
    def folded: Boolean = assignment.isFolded
