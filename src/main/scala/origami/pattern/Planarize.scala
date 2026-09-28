package origami.pattern

import origami.geometry.{Seg, Tol}
import origami.pattern.Assignment.{Boundary, Unassigned}
import origami.utils.SeqUtils.*

import scala.collection.mutable

/** Cuts every crease where it meets another, and merges points that coincide. */
object Planarize:

  def apply(cp: CreasePattern)(using tol: Tol): PlanarGraph =
    val segments = (cp.paper.edges.map(s => (s, Boundary)) ++ cp.creases.map(c => (c.seg, c.assignment)))
      .filterNot(_._1.isDegenerate)
    val cuts = cutParameters(segments.map(_._1))

    val points = PointIndex(tol)
    val edges = mutable.LinkedHashMap.empty[(Int, Int), Assignment]
    for ((seg, assignment), segCuts) <- segments.zip(cuts); (t0, t1) <- pieces(seg, segCuts) do
      val (u, v) = (points.indexOf(seg.at(t0)), points.indexOf(seg.at(t1)))
      if u != v then edges.updateWith(Edge.key(u, v))(existing => Some(merge(existing, assignment)))

    PlanarGraph(points.all, edges.toVector.map((ends, assignment) => Edge(ends._1, ends._2, assignment)), cp.paper)

  /** Where each segment must be cut: where another crosses it, and where another's end touches it. */
  private def cutParameters(segs: Vector[Seg])(using Tol): Vector[Vector[Double]] =
    val cuts = Vector.fill(segs.length)(mutable.ArrayBuffer.empty[Double])
    for i <- segs.indices; j <- (i + 1) until segs.length do
      val (si, sj) = (segs(i), segs(j))
      si.intersect(sj).foreach: p =>
        cuts(i) += si.paramOf(p)
        cuts(j) += sj.paramOf(p)
      for p <- Vector(sj.a, sj.b) if si.contains(p) do cuts(i) += si.paramOf(p)
      for p <- Vector(si.a, si.b) if sj.contains(p) do cuts(j) += sj.paramOf(p)
    cuts.map(_.toVector)

  /** The pieces of a segment between consecutive cuts, as parameter ranges. */
  private def pieces(s: Seg, cuts: Vector[Double])(using tol: Tol): Iterator[(Double, Double)] =
    val eps = tol.value / math.max(s.length, tol.value)
    val ts = (Vector(0.0, 1.0) ++ cuts).map(_.max(0.0).min(1.0)).sorted
    ts.distinctConsecutiveWith((a, b) => math.abs(a - b) <= eps).sliding(2).collect { case Vector(t0, t1) => (t0, t1) }

  /** Creases drawn on top of each other are one crease: the boundary wins, then the first assigned one. */
  private def merge(existing: Option[Assignment], incoming: Assignment): Assignment =
    existing match
      case None                            => incoming
      case Some(_) if incoming == Boundary => Boundary
      case Some(Unassigned)                => incoming
      case Some(current)                   => current
