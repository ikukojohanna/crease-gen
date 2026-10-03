package origami.folding

import origami.geometry.{Polygon, Pt, Tol}
import origami.laws.{Assigner, Laws, Violation}
import origami.pattern.{Assignment, CreasePattern, PlanarGraph}

/** A pattern with a valid layer order; only `of` and `searchAll` build one. */
final case class FoldedModel private (
    graph: PlanarGraph, state: FoldedState, stacking: Stacking, overlaps: Int):
  def facets: Int = state.maps.length
  def thickness(using Tol): Int = Layers.maxDepth(state)
  def foldedFacets: Vector[Polygon] = state.foldedFacets
  def bottomUp: Vector[Int] = stacking.order

object FoldedModel:

  /** Folds `g`. Unassigned creases fold, and the layer order decides whether each is a mountain or a valley. */
  def of(g: PlanarGraph, budget: Int = 400000)(using Tol): Either[Rejection, FoldedModel] =
    val unlabelled = g.edges.exists(_.assignment == Assignment.Unassigned)
    for
      _ <- noViolations(if unlabelled then Laws.checkGeometry(g) else Laws.checkAll(g))
      state <- FoldedState.from(g).left.map(Rejection.LocalLaws(_))
      model <- Layers.solve(state, budget) match
        case LayerVerdict.Stacked(s, o, _) =>
          val labelled = Layers.label(state, s)
          noViolations(Laws.checkAll(labelled)).map(_ => FoldedModel(labelled, state, s, o))
        case other => Left(Rejection.Layers(other))
    yield model

  def of(cp: CreasePattern)(using Tol): Either[Rejection, FoldedModel] = of(cp.planarize)

  private def noViolations(vs: Vector[Violation]): Either[Rejection, Unit] =
    if vs.isEmpty then Right(()) else Left(Rejection.LocalLaws(vs))

  /** The best of the distinct flat foldings a search found, fewest layers first. */
  final case class Ranked(models: Vector[FoldedModel], found: Int, tried: Int, exhausted: Boolean):
    def best: FoldedModel = models.head

  /** Tries every way to leave the optional creases flat or folded, for `rankMillis` after the first folding
    * or until the choices run out, keeping the `keep` best by thickness, then overlaps. Symmetric choices only,
    * unless none of them fold.
    */
  def searchAll(g: PlanarGraph, symmetries: Vector[Pt => Pt] = Vector.empty, keep: Int = 12,
      millis: Long = 30000, rankMillis: Long = 5000)(using Tol): Either[Rejection, Ranked] =
    var deadline = System.currentTimeMillis + millis
    var tried = 0

    def collect(symmetries: Vector[Pt => Pt]): Ranked =
      var best = Vector.empty[(FoldedModel, (Int, Int))]
      var found = 0
      val choices = Assigner.choices(g, symmetries)
      while choices.hasNext && System.currentTimeMillis <= deadline do
        tried += 1
        of(choices.next()).foreach: m =>
          if found == 0 then deadline = math.min(deadline, System.currentTimeMillis + rankMillis)
          found += 1
          best = (best :+ (m, (m.thickness, m.overlaps))).sortBy(_._2).take(keep)
      Ranked(best.map(_._1), found, tried, exhausted = !choices.hasNext)

    val symmetric = Option.when(symmetries.nonEmpty)(collect(symmetries)).filter(_.found > 0)
    val ranked = symmetric.getOrElse(collect(Vector.empty))
    if ranked.found == 0 then Left(Rejection.NoLabelling(tried)) else Right(ranked)
