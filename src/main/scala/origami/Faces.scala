package origami

import scala.collection.mutable

/** One facet of paper: a region of the sheet bounded by creases and edges,
  * which folding moves around rigidly and never bends.
  *
  * `vertices` are indices into the crease graph and `polygon` the same corners
  * as points, counter-clockwise, in the same order.
  */
final case class Facet(index: Int, vertices: Vector[Int], polygon: Polygon):
  def centroid: Pt =
    val vs = polygon.vertices
    val n = vs.length
    var (cx, cy, area) = (0.0, 0.0, 0.0)
    for i <- 0 until n do
      val (p, q) = (vs(i), vs((i + 1) % n))
      val cross = p.x * q.y - q.x * p.y
      area += cross
      cx += (p.x + q.x) * cross
      cy += (p.y + q.y) * cross
    if math.abs(area) < 1e-12 then vs.head
    else Pt(cx / (3 * area), cy / (3 * area))

/** The crease pattern cut into facets.
  *
  * A crease pattern is a drawing until you find its faces. After that it is a
  * collection of rigid pieces hinged together, which is the thing you can
  * actually fold.
  *
  * Faces are found by walking half-edges: from a dart, the next dart of the
  * same face is the one immediately clockwise from the dart you came back
  * along. Traversing that way traces bounded faces counter-clockwise, so the
  * one face that comes out with negative area is the outside world.
  */
final case class FaceGraph(graph: PlanarGraph, facets: Vector[Facet], faceOfDart: Vector[Int]):
  def dartsOf(edge: Int): (Int, Int) = (2 * edge, 2 * edge + 1)

  /** The one or two facets meeting along an edge. */
  def facesAt(edge: Int): Vector[Int] =
    Vector(faceOfDart(2 * edge), faceOfDart(2 * edge + 1)).filter(_ >= 0).distinct

  /** Edges an edge away from this facet, with the facet on the other side. */
  def neighbours(face: Int): Vector[(Int, Int)] =
    graph.edges.indices.toVector.flatMap: e =>
      facesAt(e) match
        case Vector(x, y) if x == face => Some((e, y))
        case Vector(x, y) if y == face => Some((e, x))
        case _                         => None

  def edgeSeg(edge: Int): Seg =
    val e = graph.edges(edge)
    Seg(graph.vertices(e.u), graph.vertices(e.v))

object Faces:

  def apply(g: PlanarGraph)(using tol: Tol): FaceGraph =
    val nDarts = 2 * g.edges.length
    def from(d: Int): Int = if d % 2 == 0 then g.edges(d / 2).u else g.edges(d / 2).v
    def to(d: Int): Int = from(d ^ 1)

    // Darts leaving each vertex, counter-clockwise by direction.
    val ring = Vector.fill(g.vertices.length)(mutable.ArrayBuffer.empty[Int])
    for d <- 0 until nDarts do ring(from(d)) += d
    val rings = ring.map(_.sortBy(d => (g.vertices(to(d)) - g.vertices(from(d))).angle).toVector)
    val slot = Array.fill(nDarts)(0)
    rings.foreach(r => r.zipWithIndex.foreach((d, i) => slot(d) = i))

    // The next dart of the face: come back along the edge, then turn clockwise.
    def next(d: Int): Int =
      val back = d ^ 1
      val r = rings(from(back))
      r((slot(back) - 1 + r.length) % r.length)

    val faceOfDart = Array.fill(nDarts)(-1)
    val cycles = mutable.ArrayBuffer.empty[Vector[Int]]
    for start <- 0 until nDarts if faceOfDart(start) == -1 do
      val cycle = mutable.ArrayBuffer.empty[Int]
      var d = start
      while faceOfDart(d) == -1 do
        faceOfDart(d) = cycles.length
        cycle += d
        d = next(d)
      cycles += cycle.toVector

    // The corners of each face, in order. A face that runs along both sides of a
    // dangling crease really does visit a vertex twice -- the walk goes up one
    // side of the slit and back down the other -- so the repeat stays. Dropping
    // it would cut the corner off the face and lose the area with it.
    val outlines = cycles.map(c => c.map(from))

    // Bounded faces run counter-clockwise; the outside runs the other way.
    val keep = cycles.indices.filter: i =>
      outlines(i).length >= 3 && signedArea(outlines(i).map(g.vertices)) > tol.value

    val renumber = keep.zipWithIndex.toMap
    val facets = keep.zipWithIndex.map: (old, i) =>
      Facet(i, outlines(old), Polygon(outlines(old).map(g.vertices)))

    FaceGraph(g, facets.toVector, faceOfDart.toVector.map(f => renumber.getOrElse(f, -1)))

  private def signedArea(vs: Vector[Pt]): Double =
    vs.indices.map { i =>
      val (p, q) = (vs(i), vs((i + 1) % vs.length))
      p.x * q.y - q.x * p.y
    }.sum / 2

