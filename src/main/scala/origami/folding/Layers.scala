package origami.folding

import origami.geometry.{Pt, Tol, Triangulate}
import origami.pattern.{Assignment, PlanarGraph}

/** Which facet lies on top of which: Justin's taco-taco, taco-tortilla and tortilla-tortilla conditions. */
object Layers:

  final case class Conditions(tacoTaco: Boolean = true, tacoTortilla: Boolean = true,
      tortillaTortilla: Boolean = true)

  def solve(state: FoldedState, budget: Int = 400000,
      conditions: Conditions = Conditions())(using Tol): LayerVerdict =
    if state.faces.facets.isEmpty then LayerVerdict.Stacked(Stacking(Vector.empty), 0, 0)
    else
      val problem = StackingProblem(state, conditions)
      val overlaps = problem.overlappingPairs.length
      val constraints = problem.constraints.length
      val solver = LayerSolver(problem.facetCount, problem.constraints, problem.overlappingPairs, problem.forced,
        budget)
      solver.run() match
        case Some(stacking)        => LayerVerdict.Stacked(stacking, overlaps, constraints)
        case None if solver.gaveUp => LayerVerdict.GaveUp(solver.steps, overlaps, constraints)
        case None                  => LayerVerdict.Impossible(overlaps, constraints)

  /** Each unassigned crease labelled by the stacking: valley if it brings the moving facet on top of a
    * face-up sheet, mountain otherwise.
    */
  def label(state: FoldedState, stacking: Stacking): PlanarGraph =
    state.graph.withAssignments: (e, edge) =>
      state.faces.facesAt(e) match
        case Vector(left, right) if edge.assignment == Assignment.Unassigned =>
          val rightOnTop = stacking.layerOf(right) > stacking.layerOf(left)
          if rightOnTop == state.maps(left).facesUp then Assignment.Valley else Assignment.Mountain
        case _ => edge.assignment

  /** How many layers lie over the thickest point of the model, sampled on a grid. */
  def maxDepth(state: FoldedState, samples: Int = 160)(using Tol): Int =
    val triangles = state.foldedFacets.map(Triangulate.apply)
    val b = state.bounds
    val eps = math.max(b.width, b.height) * 1e-9
    def depthAt(p: Pt): Int = triangles.count(_.exists(_.strictlyContains(p, eps)))
    val grid =
      for i <- 0 until samples; j <- 0 until samples
      yield Pt(b.lo.x + b.width * (i + 0.37) / samples, b.lo.y + b.height * (j + 0.61) / samples)
    grid.map(depthAt).maxOption.getOrElse(0)
