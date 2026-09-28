package origami.laws

import origami.geometry.{Pt, Tol}
import origami.pattern.{Assignment, Edge, PlanarGraph}
import origami.utils.UnionFind

object Assigner:

  /** A set of creases that must get the same label. */
  type Unknown = Vector[Int]

  /** Every labelling that passes the local laws, lazily. Symmetric ones only, if the symmetries allow it. */
  def labellings(g: PlanarGraph, symmetries: Vector[Pt => Pt] = Vector.empty)(using Tol): Iterator[PlanarGraph] =
    val unknowns = symmetricUnknowns(g, symmetries).getOrElse(eachCreaseAlone(g))
    LabellingSearch(g, unknowns).results

  def solve(g: PlanarGraph)(using Tol): Option[PlanarGraph] =
    LabellingSearch(g, eachCreaseAlone(g)).results.nextOption()

  def solvePreferSymmetric(g: PlanarGraph, symmetries: Vector[Pt => Pt])(using Tol): Option[PlanarGraph] =
    symmetricUnknowns(g, symmetries)
      .flatMap(unknowns => LabellingSearch(g, unknowns).results.nextOption())
      .orElse(solve(g))

  /** Forget every mountain and valley, keeping only the geometry. */
  def blank(g: PlanarGraph): PlanarGraph =
    g.withAssignments: (_, e) =>
      e.assignment match
        case Assignment.Boundary | Assignment.Flat => e.assignment
        case _                                     => Assignment.Unassigned

  private def isUnassigned(g: PlanarGraph)(e: Int): Boolean = g.assignment(e) == Assignment.Unassigned

  private def eachCreaseAlone(g: PlanarGraph): Vector[Unknown] =
    g.edges.indices.filter(isUnassigned(g)).map(Vector(_)).toVector

  /** Creases a symmetry maps onto each other share one unknown. None if a symmetry maps a crease off the pattern. */
  private def symmetricUnknowns(g: PlanarGraph, symmetries: Vector[Pt => Pt])(using Tol): Option[Vector[Unknown]] =
    val images = for (e, i) <- g.edges.zipWithIndex; symmetry <- symmetries yield (i, image(g, e, symmetry))
    Option.when(symmetries.nonEmpty && images.forall(_._2.isDefined)):
      val orbits = UnionFind(g.edges.length)
      images.foreach((i, j) => orbits.union(i, j.get))
      g.edges.indices.filter(isUnassigned(g)).groupBy(orbits.find).values.map(_.toVector).toVector.sortBy(_.head)

  /** The edge a symmetry maps `e` onto, if there is one with the same assignment. */
  private def image(g: PlanarGraph, e: Edge, symmetry: Pt => Pt)(using Tol): Option[Int] =
    def vertexAt(p: Pt): Option[Int] = Some(g.vertices.indexWhere(_ ~= p)).filter(_ >= 0)
    for
      u <- vertexAt(symmetry(g.vertices(e.u)))
      v <- vertexAt(symmetry(g.vertices(e.v)))
      j <- Some(g.edges.indexWhere(_.key == Edge.key(u, v))).filter(_ >= 0)
      if g.edges(j).assignment == e.assignment
    yield j
