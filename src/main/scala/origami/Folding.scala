package origami

/** A crease pattern that has been folded: the geometry, the folded position of
  * every facet, and an order for the layers.
  *
  * This is the strongest claim the program can make, and like `FlatFoldable` it
  * is a claim you can only hold by having earned it -- the constructor is
  * private and the only ways in are `of`, which checks, and `search`, which
  * hunts. The difference between the two types is the whole second half of the
  * story: `FlatFoldable` means every vertex is happy, `FoldedModel` means the
  * paper is.
  */
final case class FoldedModel private (
    graph: PlanarGraph, state: FoldedState, stacking: Stacking, overlaps: Int):
  def facets: Int = state.maps.length
  /** Layers at the thickest point of the model -- how many sheets your fingers
    * would feel there. */
  def thickness(using Tol): Int = Layers.maxDepth(state)
  def foldedFacets: Vector[Polygon] = state.foldedFacets
  /** Facets from the bottom of the stack upwards. */
  def bottomUp: Vector[Int] = stacking.order

object FoldedModel:

  /** Fold this exact pattern, with this exact labelling, or say why not. */
  def of(g: PlanarGraph, budget: Int = 400000)(using Tol): Either[Rejection, FoldedModel] =
    Laws.checkAll(g) match
      case violations if violations.nonEmpty => Left(Rejection.LocalLaws(violations))
      case _ =>
        FoldedState.from(g) match
          case Left(violations) => Left(Rejection.LocalLaws(violations))
          case Right(state) =>
            Layers.solve(state, budget) match
              case LayerVerdict.Stacked(s, o, _) => Right(FoldedModel(g, state, s, o))
              case other                         => Left(Rejection.Layers(other))

  def of(cp: CreasePattern)(using Tol): Either[Rejection, FoldedModel] = of(cp.planarize)

  /** Try labellings until one actually folds.
    *
    * The local laws leave thousands of ways to label a pattern and most of them
    * are lies. This is the only way the program has of telling the difference,
    * and it is a search, not a check, because the underlying problem is
    * NP-hard.
    */
  def search(g: PlanarGraph, symmetries: Vector[Pt => Pt] = Vector.empty,
      limit: Int = 400000, millis: Long = 30000)(using Tol): Either[Rejection, (FoldedModel, Int)] =
    val blank = Assigner.blank(g)
    val deadline = System.currentTimeMillis + millis
    var model: Option[FoldedModel] = None
    var tried = 0

    def sweep(syms: Vector[Pt => Pt]): Unit =
      if model.isEmpty then
        Assigner.findSolution(blank, limit, syms): candidate =>
          tried += 1
          if System.currentTimeMillis > deadline then true
          else
            of(candidate) match
              case Right(m) => model = Some(m); true
              case Left(_)  => false
        ()

    // A symmetric sheet usually wants a symmetric folding, and there are
    // vastly fewer of those, so look there first.
    if symmetries.nonEmpty then sweep(symmetries)
    sweep(Vector.empty)

    model match
      case Some(m) => Right((m, tried))
      case None    => Left(Rejection.NoLabelling(tried))

/** Why the paper said no. */
enum Rejection:
  case LocalLaws(violations: Vector[Violation])
  case Layers(verdict: LayerVerdict)
  case NoLabelling(searched: Int)

  def explain: String = this match
    case LocalLaws(vs)  => s"${vs.length} local violation(s): ${vs.head.explain}"
    case Layers(v)      => v.describe
    case NoLabelling(n) => s"searched $n labelling(s), none of them folds"
