package origami

import origami.folding.{FoldedModel, Rejection}
import origami.geometry.Tol
import origami.laws.{Assigner, FlatFoldable}
import origami.library.{Model, Patterns}
import origami.output.{Fold, FoldedOptions, FoldedSvg, Svg, SvgOptions}
import origami.pattern.{Assignment, PlanarGraph}

import java.nio.file.{Files, Path}

object Main:

  private val NameWidth = 24
  private val Indent = " " * NameWidth

  private final case class Folded(model: FoldedModel, labellingsTried: Option[Int])

  def main(args: Array[String]): Unit =
    val outDir = Path.of(args.headOption.getOrElse("out"))
    Files.createDirectories(outDir)
    val models = selectModels(args.drop(1).toSet)

    println(s"folding -> ${outDir.toAbsolutePath}\n")
    val foldedCount = models.count(model => process(model, outDir))
    println(s"$foldedCount of ${models.length} models fold.")
gi
  private def selectModels(names: Set[String])(using Tol): Vector[Model] =
    Patterns.all.filter(m => names.isEmpty || names.contains(m.name))

  /** Returns whether the model folds. */
  private def process(model: Model, outDir: Path)(using Tol): Boolean =
    val graph = model.pattern.planarize
    printHeader(model, graph)

    val labelled = labelIfNeeded(model, graph)
    reportLocalLaws(labelled)

    val (outcome, millis) = timed(fold(model, graph, labelled))
    reportFolding(outcome, millis)
    model.caveat.foreach(c => say(s"note: $c"))
    println()

    writeFiles(model, outcome, labelled, outDir)
    outcome.isRight

  private def needsLabelling(graph: PlanarGraph): Boolean =
    graph.edges.exists(_.assignment == Assignment.Unassigned)

  private def labelIfNeeded(model: Model, graph: PlanarGraph)(using Tol): PlanarGraph =
    if needsLabelling(graph) then Assigner.solvePreferSymmetric(graph, model.symmetries).getOrElse(graph)
    else graph

  private def fold(model: Model, graph: PlanarGraph, labelled: PlanarGraph)(using Tol): Either[Rejection, Folded] =
    if needsLabelling(graph) then
      FoldedModel.search(graph, model.symmetries).map((m, tried) => Folded(m, Some(tried)))
    else FoldedModel.of(labelled).map(Folded(_, None))

  private def printHeader(model: Model, graph: PlanarGraph)(using Tol): Unit =
    println(model.name.padTo(NameWidth, ' ') + graph.summary)
    if model.note.nonEmpty then say(model.note)

  private def reportLocalLaws(graph: PlanarGraph)(using Tol): Unit =
    FlatFoldable.from(graph) match
      case Right(_) => say("✓ local laws hold at every interior vertex")
      case Left(violations) =>
        say(s"✗ ${violations.length} local violation(s)")
        violations.take(3).foreach(v => say(s"  - ${v.explain}"))

  private def reportFolding(outcome: Either[Rejection, Folded], millis: Long)(using Tol): Unit =
    outcome match
      case Right(Folded(m, tried)) =>
        tried.foreach(n => say(s"searched $n labelling(s) for one that folds"))
        say(s"✓ folds: ${m.facets} facets, ${m.thickness} layers thick, ${m.overlaps} overlapping pairs (${millis}ms)")
      case Left(why) =>
        say(s"✗ does not fold: ${why.explain} (${millis}ms)")

  private def writeFiles(model: Model, outcome: Either[Rejection, Folded], labelled: PlanarGraph, outDir: Path)(
      using Tol): Unit =
    val graph = outcome.map(_.model.graph).getOrElse(labelled)
    val status = if outcome.isRight then "✓ folds" else "✗ does not fold"
    val patternSvg = Svg.render(graph.toPattern, SvgOptions(scale = 1.6, title = Some(s"${model.name}  —  $status")))
    Files.writeString(outDir.resolve(s"${model.name}.svg"), patternSvg)
    Files.writeString(outDir.resolve(s"${model.name}.fold"), Fold(graph, model.name))

    val foldedSvgPath = outDir.resolve(s"${model.name}-folded.svg")
    outcome match
      case Right(Folded(m, _)) =>
        val title = s"${model.name} folded — ${m.thickness} layers thick"
        Files.writeString(foldedSvgPath, FoldedSvg.render(m, FoldedOptions(title = Some(title))))
      case Left(_) =>
        Files.deleteIfExists(foldedSvgPath)

  private def say(line: String): Unit = println(Indent + line)

  private def timed[A](work: => A): (A, Long) =
    val start = System.currentTimeMillis
    val result = work
    (result, System.currentTimeMillis - start)
