package origami.pattern

import origami.geometry.{Polygon, Pt, Tol}

import scala.collection.mutable

final case class PlanarGraph(vertices: Vector[Pt], edges: Vector[Edge], paper: Polygon):

  lazy val incident: Vector[Vector[Int]] =
    val buf = Vector.fill(vertices.length)(mutable.ArrayBuffer.empty[Int])
    edges.zipWithIndex.foreach { case (e, i) => buf(e.u) += i; buf(e.v) += i }
    buf.map(_.toVector)

  def direction(edgeIdx: Int, from: Int): Double =
    val e = edges(edgeIdx)
    (vertices(e.other(from)) - vertices(from)).angle

  def isBoundaryVertex(i: Int)(using Tol): Boolean = paper.onBoundary(vertices(i))

  def kind(i: Int)(using Tol): VertexKind =
    if isBoundaryVertex(i) then VertexKind.Boundary else VertexKind.Interior

  def info(i: Int)(using Tol): VertexInfo =
    val folded = incident(i)
      .filter(e => edges(e).assignment.isFolded || edges(e).assignment.isUndecided)
      .map(e => (e, direction(e, i)))
      .sortBy(_._2)
    VertexInfo(i, vertices(i), kind(i), folded.map(_._1), folded.map(_._2))

  def interiorVertices(using Tol): Vector[VertexInfo] =
    vertices.indices.toVector.map(info).filter(_.kind == VertexKind.Interior)

  def assignment(i: Int): Assignment = edges(i).assignment

  def withAssignments(f: (Int, Edge) => Assignment): PlanarGraph =
    copy(edges = edges.zipWithIndex.map((e, i) => e.withAssignment(f(i, e))))

  def toPattern(using Tol): CreasePattern =
    CreasePattern.blank(paper).creaseAll:
      edges.filterNot(_.assignment == Assignment.Boundary).map(e => (vertices(e.u), vertices(e.v), e.assignment))

  def summary(using Tol): String =
    val counts = edges.groupBy(_.assignment).view.mapValues(_.length).toMap
    val interior = interiorVertices.length
    s"${vertices.length} vertices ($interior interior), ${edges.length} edges " +
      Assignment.values.filter(counts.contains).map(a => s"${a.code}=${counts(a)}").mkString("[", " ", "]")
