package origami.laws

import origami.geometry.Tol
import origami.laws.Assigner.Unknown
import origami.pattern.{Assignment, PlanarGraph}
import origami.pattern.Assignment.{Mountain, Unassigned, Valley}

/** Depth-first over the unknowns, backing out as soon as a vertex can no longer satisfy Maekawa. */
private[laws] final class LabellingSearch(g: PlanarGraph, unknowns: Vector[Unknown])(using tol: Tol):
  private val vertices = g.interiorVertices
  private val littleSectors = vertices.map(_.littleSectors)
  private val neighbours = ParallelNeighbours(g)
  private val labels = g.edges.map(_.assignment).toArray

  /** Each unknown with the vertices it touches, the most entangled first. */
  private val order: List[(Unknown, Vector[Int])] =
    unknowns.map(u => (u, verticesTouching(u))).sortBy(-_._2.length).toList

  def results: Iterator[PlanarGraph] = decide(order)

  private def decide(remaining: List[(Unknown, Vector[Int])]): Iterator[PlanarGraph] = remaining match
    case Nil =>
      if vertices.indices.forall(canStillFold) then Iterator.single(g.withAssignments((i, _) => labels(i)))
      else Iterator.empty
    case (unknown, touched) :: rest =>
      preferredLabels(unknown).iterator.flatMap: label =>
        setLabel(unknown, label)
        val deeper = if touched.forall(canStillFold) then decide(rest) else Iterator.empty
        deeper ++ { setLabel(unknown, Unassigned); Iterator.empty }

  private def verticesTouching(unknown: Unknown): Vector[Int] =
    vertices.indices.filter(vi => vertices(vi).foldedEdges.exists(unknown.contains)).toVector

  private def setLabel(unknown: Unknown, label: Assignment): Unit =
    unknown.foreach(e => labels(e) = label)

  /** Disagree with the parallel neighbours first, so pleats come before rolls. */
  private def preferredLabels(unknown: Unknown): List[Assignment] =
    val around = unknown.flatMap(neighbours)
    if around.count(labels(_) == Mountain) > around.count(labels(_) == Valley) then List(Valley, Mountain)
    else List(Mountain, Valley)

  private def canStillFold(vi: Int): Boolean =
    val creases = vertices(vi).foldedEdges
    val m = creases.count(labels(_) == Mountain)
    val v = creases.count(labels(_) == Valley)
    val open = creases.length - m - v
    if open > 0 then (0 to open).exists(k => math.abs((m + k) - (v + open - k)) == 2)
    else math.abs(m - v) == 2 && littleSectors(vi).forall(s => labels(s.left) != labels(s.right))
