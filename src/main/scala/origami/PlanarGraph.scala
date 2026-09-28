package origami

import scala.collection.mutable
import scala.math.Pi

final case class Edge(u: Int, v: Int, assignment: Assignment):
  def ends: (Int, Int) = (u, v)
  def other(i: Int): Int = if i == u then v else u
  def key: (Int, Int) = if u < v then (u, v) else (v, u)
  def withAssignment(a: Assignment): Edge = Edge(u, v, a)

enum VertexKind:
  case Interior, Boundary

/** One interior vertex, seen the way the theorems see it: a cyclic sequence of
  * folded creases and the sectors of paper between them.
  */
final case class VertexInfo(
    index: Int,
    at: Pt,
    kind: VertexKind,
    /** Edge indices of the *folded* creases, counter-clockwise by direction. */
    foldedEdges: Vector[Int],
    /** Angles, in the same order. */
    directions: Vector[Double]
):
  def degree: Int = foldedEdges.length

  /** The angles of paper between consecutive creases; they sum to 2*Pi. */
  def sectors: Vector[Double] =
    if degree == 0 then Vector(2 * Pi)
    else
      directions.indices.toVector.map: i =>
        Geometry.norm2Pi(directions((i + 1) % degree) - directions(i))

/** A crease pattern after the segments have been cut at their crossings: the
  * same drawing, now a graph.
  *
  * The laws of flat folding are statements about vertices, and a vertex only
  * exists once you know which creases meet. Planarisation is the step where a
  * picture becomes a structure you can reason about.
  */
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
      .filter(e => edges(e).assignment.isFolded || edges(e).assignment == Assignment.Unassigned)
      .map(e => (e, direction(e, i)))
      .sortBy(_._2)
    VertexInfo(i, vertices(i), kind(i), folded.map(_._1), folded.map(_._2))

  def interiorVertices(using Tol): Vector[VertexInfo] =
    vertices.indices.toVector.map(info).filter(_.kind == VertexKind.Interior)

  def assignment(i: Int): Assignment = edges(i).assignment

  def withAssignments(f: (Int, Edge) => Assignment): PlanarGraph =
    copy(edges = edges.zipWithIndex.map((e, i) => e.withAssignment(f(i, e))))

  /** Back to a drawable pattern. */
  def toPattern(using Tol): CreasePattern =
    edges.foldLeft(CreasePattern.blank(paper)): (cp, e) =>
      if e.assignment == Assignment.Boundary then cp
      else cp.crease(vertices(e.u), vertices(e.v), e.assignment)

  def summary(using Tol): String =
    val counts = edges.groupBy(_.assignment).view.mapValues(_.length).toMap
    val interior = interiorVertices.length
    s"${vertices.length} vertices ($interior interior), ${edges.length} edges " +
      Assignment.values.filter(counts.contains).map(a => s"${a.code}=${counts(a)}").mkString("[", " ", "]")

/** Cut every crease at every crossing, and merge coincident points.
  *
  * Paper does this for free. Two creases that cross share a point because they
  * are the same sheet; the program has to be told.
  */
object Planarize:

  def apply(cp: CreasePattern)(using tol: Tol): PlanarGraph =
    val raw: Vector[(Seg, Assignment)] =
      cp.paper.edges.map(s => (s, Assignment.Boundary)) ++ cp.creases.map(c => (c.seg, c.assignment))

    val segs = raw.filterNot(_._1.isDegenerate(using tol))

    // Every point where a segment must be cut: crossings, and endpoints of
    // other segments lying on it (T-junctions and collinear overlaps).
    val cuts = Vector.fill(segs.length)(mutable.ArrayBuffer.empty[Double])
    for
      i <- segs.indices
      j <- (i + 1) until segs.length
    do
      val (si, sj) = (segs(i)._1, segs(j)._1)
      si.intersect(sj).foreach: p =>
        cuts(i) += si.paramOf(p)
        cuts(j) += sj.paramOf(p)
      for p <- Vector(sj.a, sj.b) if si.contains(p) do cuts(i) += si.paramOf(p)
      for p <- Vector(si.a, si.b) if sj.contains(p) do cuts(j) += sj.paramOf(p)

    val index = PointIndex(tol)
    val edgeSet = mutable.LinkedHashMap.empty[(Int, Int), Assignment]

    for i <- segs.indices do
      val (s, a) = segs(i)
      val eps = tol.value / math.max(s.length, tol.value)
      val ts = (Vector(0.0, 1.0) ++ cuts(i)).map(t => t.max(0.0).min(1.0)).sorted
      val uniq = ts.foldLeft(Vector.empty[Double])((acc, t) =>
        if acc.lastOption.exists(u => math.abs(u - t) <= eps) then acc else acc :+ t)
      for Vector(t0, t1) <- uniq.sliding(2) do
        val (u, v) = (index.add(s.at(t0)), index.add(s.at(t1)))
        if u != v then
          val k = if u < v then (u, v) else (v, u)
          edgeSet.updateWith(k)(existing => Some(merge(existing, a)))

    PlanarGraph(index.points, edgeSet.toVector.map((k, a) => Edge(k._1, k._2, a)), cp.paper)

  /** Two creases drawn on top of each other are one crease. Boundary wins,
    * then an explicit fold, then anything. */
  private def merge(existing: Option[Assignment], incoming: Assignment): Assignment =
    existing match
      case None                                => incoming
      case Some(Assignment.Boundary)           => Assignment.Boundary
      case Some(_) if incoming == Assignment.Boundary => Assignment.Boundary
      case Some(Assignment.Unassigned)         => incoming
      case Some(cur) if incoming == Assignment.Unassigned => cur
      case Some(cur)                           => cur

  /** Points within tolerance of each other are the same point. */
  private final class PointIndex(tol: Tol):
    private val cell = tol.value * 4
    private val buckets = mutable.HashMap.empty[(Long, Long), mutable.ArrayBuffer[Int]]
    private val pts = mutable.ArrayBuffer.empty[Pt]

    private def key(p: Pt): (Long, Long) =
      (math.floor(p.x / cell).toLong, math.floor(p.y / cell).toLong)

    def add(p: Pt): Int =
      val (kx, ky) = key(p)
      val hit =
        (for
          dx <- -1 to 1
          dy <- -1 to 1
          b <- buckets.get((kx + dx, ky + dy)).toSeq
          i <- b
          if pts(i).distTo(p) <= tol.value
        yield i).headOption
      hit.getOrElse:
        val i = pts.length
        pts += p
        buckets.getOrElseUpdate((kx, ky), mutable.ArrayBuffer.empty) += i
        i

    def points: Vector[Pt] = pts.toVector
