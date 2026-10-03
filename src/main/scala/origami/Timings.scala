package origami

import origami.folding.FoldedModel
import origami.geometry.{Geometry, Tol}
import origami.laws.Laws
import origami.tiling.{ShrinkRotate, Tiling, Tilings}

/** How long checking takes as twist tessellations grow: `scala-cli run . -- timings [maxSize]`. */
object Timings:

  def report(maxSize: Int)(using Tol): Vector[String] =
    warmUp()
    val kinds = Vector[(String, Int => Tiling)](
      "3.6.3.6" -> (Tilings.trihexagonal(_)), "6.6.6" -> (Tilings.hexagonal(_)),
      "4.4.4.4" -> (Tilings.square(_)), "3.3.3.3.3.3" -> (Tilings.triangular(_)))
    val header = f"${"tiling"}%-12s ${"size"}%4s ${"facets"}%6s ${"planarize"}%10s ${"laws"}%7s ${"fold"}%9s  result"
    header +: (for (name, tiling) <- kinds; size <- 1 to maxSize yield row(name, size, tiling(size)))

  private def row(name: String, size: Int, t: Tiling)(using Tol): String =
    val cp = ShrinkRotate(t, 0.5, Geometry.degrees(30)).fold(p => throw IllegalStateException(p.explain), identity)
    val (g, planar) = timed(cp.planarize)
    val (violations, laws) = timed(Laws.checkAll(g))
    val (folded, fold) = timed(FoldedModel.of(g))
    val result = folded.fold(_.explain, _ => "folds")
    f"$name%-12s $size%4d ${folded.map(_.facets).getOrElse(0)}%6d $planar%8d ms $laws%4d ms $fold%6d ms  " +
      s"$result, ${violations.length} local violation(s)"

  private def warmUp()(using Tol): Unit =
    for _ <- 1 to 3 do ShrinkRotate(Tilings.trihexagonal(1), 0.5, Geometry.degrees(30)).map(FoldedModel.of(_))

  private def timed[A](work: => A): (A, Long) =
    val start = System.nanoTime
    val result = work
    (result, (System.nanoTime - start) / 1000000)
