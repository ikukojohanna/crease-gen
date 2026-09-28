package origami.folding

import origami.geometry.{Polygon, Pt, Tol}
import origami.laws.{Assigner, Laws}
import origami.pattern.{CreasePattern, PlanarGraph}

/** A pattern with a valid layer order; only `of` and `search` build one. */
final case class FoldedModel private (
    graph: PlanarGraph, state: FoldedState, stacking: Stacking, overlaps: Int):
  def facets: Int = state.maps.length
  def thickness(using Tol): Int = Layers.maxDepth(state)
  def foldedFacets: Vector[Polygon] = state.foldedFacets
  def bottomUp: Vector[Int] = stacking.order

object FoldedModel:

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

  def search(g: PlanarGraph, symmetries: Vector[Pt => Pt] = Vector.empty,
      limit: Int = 400000, millis: Long = 30000)(using Tol): Either[Rejection, (FoldedModel, Int)] =
    val blank = Assigner.blank(g)
    val deadline = System.currentTimeMillis + millis
    var tried = 0

    def candidates(symmetries: Vector[Pt => Pt]): Iterator[PlanarGraph] =
      Assigner.labellings(blank, symmetries)
        .take(limit)
        .tapEach(_ => tried += 1)
        .takeWhile(_ => System.currentTimeMillis <= deadline)

    // Symmetric labellings first: there are far fewer of them.
    val symmetric = if symmetries.nonEmpty then candidates(symmetries) else Iterator.empty
    (symmetric ++ candidates(Vector.empty)).map(of(_)).collectFirst { case Right(m) => m } match
      case Some(m) => Right((m, tried))
      case None    => Left(Rejection.NoLabelling(tried))
