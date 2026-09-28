package origami

import java.nio.file.{Files, Path}

/** Run the whole pipeline over the library.
  *
  * For each model: cut the drawing into a graph, check the local laws, label it
  * if it is unlabelled, fold it, order the layers -- and write the crease
  * pattern to print, the folded model to look at, and a FOLD file to simulate.
  */
object Main:

  given Tol = Tol(1e-7)

  def main(args: Array[String]): Unit =
    val out = Path.of(if args.nonEmpty then args(0) else "out")
    Files.createDirectories(out)
    val wanted = args.drop(1).toSet
    val models = Patterns.all.filter(m => wanted.isEmpty || wanted.contains(m.name))

    println(s"folding -> ${out.toAbsolutePath}\n")
    val results = models.map(m => emit(out, m))
    val folded = results.count(identity)
    println(s"$folded of ${results.length} models fold.")

  private def emit(out: Path, model: Model)(using Tol): Boolean =
    val pad = " " * 24
    val graph = model.pattern.planarize
    val undecided = graph.edges.exists(_.assignment == Assignment.Unassigned)

    println(s"${model.name.padTo(24, ' ')}${graph.summary}")
    if model.note.nonEmpty then println(s"$pad${model.note}")

    // Stage one: the local laws, on the labelling we were given.
    val local =
      if undecided then
        Assigner.solvePreferSymmetric(graph, model.symmetries).getOrElse(graph)
      else graph
    FlatFoldable.from(local) match
      case Right(_) => println(s"$pad✓ local laws hold at every interior vertex")
      case Left(vs) =>
        println(s"$pad✗ ${vs.length} local violation(s)")
        vs.take(3).foreach(v => println(s"$pad  - ${v.explain}"))

    // Stage two: fold it, and order the layers.
    val started = System.currentTimeMillis
    val outcome =
      if undecided then FoldedModel.search(graph, model.symmetries).map((m, tried) => (m, Some(tried)))
      else FoldedModel.of(local).map(m => (m, None))
    val elapsed = System.currentTimeMillis - started

    val finalGraph = outcome.map(_._1.graph).getOrElse(local)

    outcome match
      case Right(found) =>
        val (m, tried) = found
        tried.foreach(n => println(s"${pad}searched $n labelling(s) for one that folds"))
        println(f"$pad✓ folds: ${m.facets} facets, ${m.thickness} layers thick, " +
          f"${m.overlaps} overlapping pairs (${elapsed}ms)")
        write(out.resolve(s"${model.name}-folded.svg"),
          FoldedSvg.render(m, FoldedOptions(
            title = Some(s"${model.name} folded — ${m.thickness} layers thick"))))
      case Left(why) =>
        println(s"$pad✗ does not fold: ${why.explain} (${elapsed}ms)")
        Files.deleteIfExists(out.resolve(s"${model.name}-folded.svg"))

    model.caveat.foreach(c => println(s"${pad}note: $c"))
    println()

    val status = outcome.fold(_ => "✗ does not fold", _ => "✓ folds")
    write(out.resolve(s"${model.name}.svg"),
      Svg.render(finalGraph.toPattern, SvgOptions(scale = 1.6, title = Some(s"${model.name}  —  $status"))))
    write(out.resolve(s"${model.name}.fold"), Fold(finalGraph, model.name))
    outcome.isRight

  private def write(p: Path, s: String): Unit = Files.writeString(p, s)
