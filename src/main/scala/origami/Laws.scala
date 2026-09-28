package origami

import scala.math.{abs, Pi}

/** A reason the paper says no. */
enum Violation:
  /** A crease that stops in the middle of the sheet. Paper cannot tear. */
  case DeadEnd(at: Pt, degree: Int)
  /** Maekawa: an odd number of creases can never meet at a flat-folded vertex. */
  case OddDegree(at: Pt, degree: Int)
  /** Kawasaki: alternate sectors must sum to a straight angle. */
  case Kawasaki(at: Pt, alternatingSum: Double)
  /** Maekawa: mountains minus valleys is always plus or minus two. */
  case Maekawa(at: Pt, mountains: Int, valleys: Int)
  /** Big-little-big: the creases around a strictly smallest sector must differ. */
  case BigLittleBig(at: Pt, sector: Double)
  /** A fold with no assignment; the pattern is not finished. */
  case Undecided(at: Pt)
  /** The folded position of a facet depends on the route you took to reach it.
    * Kawasaki's theorem is exactly the promise that this cannot happen. */
  case Inconsistent(at: Pt)

  def where: Pt = this match
    case DeadEnd(p, _) => p
    case OddDegree(p, _) => p
    case Kawasaki(p, _) => p
    case Maekawa(p, _, _) => p
    case BigLittleBig(p, _) => p
    case Undecided(p) => p
    case Inconsistent(p) => p

  def explain: String =
    def fmt(p: Pt) = f"(${p.x}%.2f, ${p.y}%.2f)"
    this match
      case DeadEnd(p, d)      => s"${fmt(p)}: a crease ends in mid-sheet (degree $d)"
      case OddDegree(p, d)    => s"${fmt(p)}: $d creases meet, but Maekawa needs an even number"
      case Kawasaki(p, s)     => f"${fmt(p)}: Kawasaki fails, alternating sum ${s * 180 / Pi}%.3f deg (must be 0)"
      case Maekawa(p, m, v)   => s"${fmt(p)}: Maekawa fails, $m mountains and $v valleys (|M-V| must be 2)"
      case BigLittleBig(p, a) => f"${fmt(p)}: the smallest sector (${a * 180 / Pi}%.1f deg) is bounded by two creases of the same kind"
      case Undecided(p)       => s"${fmt(p)}: an unassigned crease meets this vertex"
      case Inconsistent(p)    => s"${fmt(p)}: the folding map disagrees with itself across this crease"

/** The three local laws of flat folding, checked where they apply.
  *
  * Two of them are theorems about the *geometry* alone -- they hold whatever
  * you decide to do with the creases. The third is about the assignment. The
  * separation matters: you can draw a foldable figure and still label it
  * unfoldably.
  */
object Laws:

  /** Kawasaki-Justin. Around an interior vertex the sectors alternately add and
    * subtract to nothing. Equivalently: walking around the vertex in the folded
    * state, you must come back to where you started.
    */
  def kawasaki(v: VertexInfo)(using tol: Tol): Option[Violation] =
    if v.degree == 0 then None
    else if v.degree % 2 != 0 then Some(Violation.OddDegree(v.at, v.degree))
    else
      val s = v.sectors.zipWithIndex.map((a, i) => if i % 2 == 0 then a else -a).sum
      Option.unless(tol.isZero(s))(Violation.Kawasaki(v.at, s))

  /** Maekawa-Justin. Mountains minus valleys is exactly plus or minus two,
    * because the folded cross-section of the paper is a closed zigzag.
    */
  def maekawa(g: PlanarGraph, v: VertexInfo): Option[Violation] =
    if v.degree == 0 then None
    else
      val kinds = v.foldedEdges.map(e => g.edges(e).assignment)
      val m = kinds.count(_ == Assignment.Mountain)
      val n = kinds.count(_ == Assignment.Valley)
      Option.unless(abs(m - n) == 2)(Violation.Maekawa(v.at, m, n))

  /** Big-little-big. A sector strictly smaller than both its neighbours gets
    * squeezed between its two creases; if those creases folded the same way the
    * paper would have to pass through itself.
    */
  def bigLittleBig(g: PlanarGraph, v: VertexInfo)(using tol: Tol): Vector[Violation] =
    if v.degree < 4 then Vector.empty
    else
      val s = v.sectors
      val n = s.length
      (0 until n).toVector.flatMap: i =>
        val (prev, here, next) = (s((i + n - 1) % n), s(i), s((i + 1) % n))
        // sector i lies between crease i and crease i+1
        val left = g.edges(v.foldedEdges(i)).assignment
        val right = g.edges(v.foldedEdges((i + 1) % n)).assignment
        Option.when(tol.lt(here, prev) && tol.lt(here, next) && left == right && left.isFolded)(
          Violation.BigLittleBig(v.at, here))

  def undecided(g: PlanarGraph, v: VertexInfo): Option[Violation] =
    Option.when(v.foldedEdges.exists(e => g.edges(e).assignment == Assignment.Unassigned))(
      Violation.Undecided(v.at))

  def deadEnd(v: VertexInfo): Option[Violation] =
    Option.when(v.degree == 1)(Violation.DeadEnd(v.at, v.degree))

  /** Geometry only: is this figure foldable by *some* assignment? */
  def checkGeometry(g: PlanarGraph)(using Tol): Vector[Violation] =
    g.interiorVertices.flatMap(v => deadEnd(v).toVector ++ kawasaki(v).toVector)

  /** Geometry and assignment together. */
  def checkAll(g: PlanarGraph)(using Tol): Vector[Violation] =
    g.interiorVertices.flatMap: v =>
      undecided(g, v).toVector ++ deadEnd(v).toVector ++ kawasaki(v).toVector ++
        maekawa(g, v).toVector ++ bigLittleBig(g, v)

/** A crease pattern that has passed the laws.
  *
  * The point of the opaque type is that there is no other way to make one. A
  * `FlatFoldable` in your hand is a proof that every interior vertex satisfies
  * Kawasaki, Maekawa and big-little-big -- carried in the type, checked once,
  * at the boundary, and never rechecked downstream.
  *
  * What it does *not* prove: that the sheet can be folded without passing
  * through itself globally. The local laws are necessary, not sufficient.
  * Honest types say what they know.
  */
opaque type FlatFoldable = PlanarGraph

object FlatFoldable:
  def from(g: PlanarGraph)(using Tol): Either[Vector[Violation], FlatFoldable] =
    Laws.checkAll(g) match
      case v if v.isEmpty => Right(g)
      case v              => Left(v)

  def from(cp: CreasePattern)(using Tol): Either[Vector[Violation], FlatFoldable] =
    from(cp.planarize)

  extension (f: FlatFoldable)
    def graph: PlanarGraph = f
    def pattern(using Tol): CreasePattern = f.toPattern
