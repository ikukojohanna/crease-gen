package origami.laws

import origami.geometry.{Pt, Tol}
import origami.pattern.{Assignment, Edge, PlanarGraph}
import origami.utils.UnionFind

object Assigner:

  /** A set of creases that must get the same label. */
  type Unknown = Vector[Int]

  /** Every way to leave each optional crease flat or folded, flat first. A folded one becomes unassigned, for
    * the layer order to make a mountain or a valley. Symmetric choices only, if the symmetries allow it.
    */
  def choices(g: PlanarGraph, symmetries: Vector[Pt => Pt] = Vector.empty)(using Tol): Iterator[PlanarGraph] =
    val unknowns = symmetricUnknowns(g, symmetries).getOrElse(eachCreaseAlone(g)).toList
    def settle(rest: List[Unknown], chosen: Map[Int, Assignment]): Iterator[PlanarGraph] = rest match
      case Nil => Iterator.single(g.withAssignments((i, e) => chosen.getOrElse(i, e.assignment)))
      case unknown :: more =>
        Iterator(Assignment.Flat, Assignment.Unassigned).flatMap(a => settle(more, chosen ++ unknown.map(_ -> a)))
    settle(unknowns, Map.empty)

  private def isOptional(g: PlanarGraph)(e: Int): Boolean = g.assignment(e) == Assignment.Optional

  private def eachCreaseAlone(g: PlanarGraph): Vector[Unknown] =
    g.edges.indices.filter(isOptional(g)).map(Vector(_)).toVector

  /** Creases a symmetry maps onto each other share one unknown. None if a symmetry maps a crease off the pattern. */
  private def symmetricUnknowns(g: PlanarGraph, symmetries: Vector[Pt => Pt])(using Tol): Option[Vector[Unknown]] =
    val images = for (e, i) <- g.edges.zipWithIndex; symmetry <- symmetries yield (i, image(g, e, symmetry))
    Option.when(symmetries.nonEmpty && images.forall(_._2.isDefined)):
      val orbits = UnionFind(g.edges.length)
      images.foreach((i, j) => orbits.union(i, j.get))
      g.edges.indices.filter(isOptional(g)).groupBy(orbits.find).values.map(_.toVector).toVector.sortBy(_.head)

  /** The edge a symmetry maps `e` onto, if there is one with the same assignment. */
  private def image(g: PlanarGraph, e: Edge, symmetry: Pt => Pt)(using Tol): Option[Int] =
    def vertexAt(p: Pt): Option[Int] = Some(g.vertices.indexWhere(_ ~= p)).filter(_ >= 0)
    for
      u <- vertexAt(symmetry(g.vertices(e.u)))
      v <- vertexAt(symmetry(g.vertices(e.v)))
      j <- Some(g.edges.indexWhere(_.key == Edge.key(u, v))).filter(_ >= 0)
      if g.edges(j).assignment == e.assignment
    yield j
