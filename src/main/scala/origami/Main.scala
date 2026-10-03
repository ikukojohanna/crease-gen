package origami

import origami.folding.{FoldedModel, Rejection}
import origami.geometry.Tol
import origami.laws.FlatFoldable
import origami.library.{Model, Patterns}
import origami.output.{Fold, FoldedOptions, FoldedSvg, Svg, SvgOptions}
import origami.pattern.{Assignment, PlanarGraph}
import origami.studio.Studio

import java.nio.file.{Files, Path}

object Main:

  private val NameWidth = 24
  private val Indent = " " * NameWidth

  /** How many of the searched foldings to write out and list. */
  private val Alternatives = 3

  private final case class Folded(model: FoldedModel, search: Option[FoldedModel.Ranked])

  def main(args: Array[String]): Unit =
    args.headOption match
      case Some("timings") => timings(args.lift(1).map(_.toInt).getOrElse(5))
      case Some("serve")   => Studio.serve(args.lift(1).flatMap(_.toIntOption).getOrElse(8080))
      case _               => renderAll(args)

  /** Prints the timing table and writes it to out/timings.txt. */
  private def timings(maxSize: Int): Unit =
    val lines = Timings.report(maxSize).map(line => { println(line); line })
    Files.createDirectories(Path.of("out"))
    Files.writeString(Path.of("out", "timings.txt"), lines.mkString("", "\n", "\n"))

  private def renderAll(args: Array[String]): Unit =
    val outDir = Path.of(args.headOption.getOrElse("out"))
    Files.createDirectories(outDir)
    val models = selectModels(args.drop(1).toSet)

    println(s"folding -> ${outDir.toAbsolutePath}\n")
    val foldedCount = models.count(model => process(model, outDir))
    println(s"$foldedCount of ${models.length} models fold.")

  private def selectModels(names: Set[String])(using Tol): Vector[Model] =
    Patterns.all.filter(m => names.isEmpty || names.contains(m.name))

  /** Returns whether the model folds. */
  private def process(model: Model, outDir: Path)(using Tol): Boolean =
    val graph = model.pattern.planarize
    printHeader(model, graph)

    val (outcome, millis) = timed(fold(model, graph))
    val labelled = outcome.map(_.model.graph).getOrElse(graph)
    reportLocalLaws(labelled)
    reportFolding(outcome, millis)
    model.caveat.foreach(c => say(s"note: $c"))
    println()

    writeFiles(model, outcome, labelled, outDir)
    outcome.isRight

  private def fold(model: Model, graph: PlanarGraph)(using Tol): Either[Rejection, Folded] =
    if graph.edges.exists(_.assignment == Assignment.Optional) then
      FoldedModel.searchAll(graph, model.symmetries).map(ranked => Folded(ranked.best, Some(ranked)))
    else FoldedModel.of(graph).map(Folded(_, None))

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
      case Right(Folded(m, search)) =>
        search.foreach(reportSearch)
        say(s"✓ folds: ${m.facets} facets, ${m.thickness} layers thick, ${m.overlaps} overlapping pairs (${millis}ms)")
      case Left(why) =>
        say(s"✗ does not fold: ${why.explain} (${millis}ms)")

  private def reportSearch(ranked: FoldedModel.Ranked)(using Tol): Unit =
    val stopped = if ranked.exhausted then "" else ", search stopped early"
    say(s"tried ${ranked.tried} ways to leave the optional creases flat or folded: ${ranked.found} fold$stopped, fewest layers first")
    say("! labels chosen by search: one of several flat foldings of these lines, not a specific model")
    ranked.models.take(Alternatives).zipWithIndex.foreach: (m, i) =>
      say(s"  #${i + 1}: ${m.thickness} layers thick, ${m.overlaps} overlapping pairs")
    if ranked.found > Alternatives then say(s"  … ${ranked.found - Alternatives} more")

  /** The best folding as `<name>.*`; further searched foldings as `<name>-2.*`, `<name>-3.*`. */
  private def writeFiles(model: Model, outcome: Either[Rejection, Folded], labelled: PlanarGraph, outDir: Path)(
      using Tol): Unit =
    outcome match
      case Right(Folded(best, search)) =>
        writeOne(model.name, best.graph, Some(best), outDir)
        val others = search.toVector.flatMap(_.models.drop(1).take(Alternatives - 1))
        for k <- 2 to Alternatives do
          val name = s"${model.name}-$k"
          others.lift(k - 2) match
            case Some(m) => writeOne(name, m.graph, Some(m), outDir)
            case None    => Vector(".svg", ".fold", "-folded.svg").foreach(ext => Files.deleteIfExists(outDir.resolve(name + ext)))
      case Left(_) =>
        writeOne(model.name, labelled, None, outDir)

  private def writeOne(name: String, graph: PlanarGraph, folded: Option[FoldedModel], outDir: Path)(using Tol): Unit =
    val status = if folded.isDefined then "✓ folds" else "✗ does not fold"
    val patternSvg = Svg.render(graph.toPattern, SvgOptions(scale = 1.6, title = Some(s"$name  —  $status")))
    Files.writeString(outDir.resolve(s"$name.svg"), patternSvg)
    Files.writeString(outDir.resolve(s"$name.fold"), Fold(graph, name))

    val foldedSvgPath = outDir.resolve(s"$name-folded.svg")
    folded match
      case Some(m) =>
        val title = s"$name folded — ${m.thickness} layers thick"
        Files.writeString(foldedSvgPath, FoldedSvg.render(m, FoldedOptions(title = Some(title))))
      case None =>
        Files.deleteIfExists(foldedSvgPath)

  private def say(line: String): Unit = println(Indent + line)

  private def timed[A](work: => A): (A, Long) =
    val start = System.currentTimeMillis
    val result = work
    (result, System.currentTimeMillis - start)
