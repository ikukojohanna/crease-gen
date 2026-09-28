package origami

/** What a crease is told to do.
  *
  * Not a boolean, not a string, not an int. A crease is mountain, valley, the
  * paper's edge, or a line that was pressed but left flat. Five cases, all of
  * them meaningful, none of them representable as anything else.
  */
enum Assignment:
  case Mountain, Valley, Boundary, Flat, Unassigned

  /** Folded creases are the ones the theorems apply to. */
  def isFolded: Boolean = this == Mountain || this == Valley
  def opposite: Assignment = this match
    case Mountain => Valley
    case Valley   => Mountain
    case other    => other

  /** Fold angle in degrees, in the FOLD file format's convention. */
  def foldAngle: Double = this match
    case Mountain => -180
    case Valley   => 180
    case _        => 0

  def code: String = this match
    case Mountain => "M"
    case Valley   => "V"
    case Boundary => "B"
    case Flat     => "F"
    case Unassigned => "U"

object Assignment:
  val mv: List[Assignment] = List(Mountain, Valley)

final case class Crease(seg: Seg, assignment: Assignment):
  def a: Pt = seg.a
  def b: Pt = seg.b
  def withAssignment(x: Assignment): Crease = Crease(seg, x)

/** A sheet of paper and the creases on it.
  *
  * The invariant, maintained by construction rather than by checking: every
  * crease lies on the paper. You cannot fold what isn't there, so the type
  * cannot hold what isn't there. Adding a crease clips it to the sheet, and a
  * fold that misses the paper entirely simply adds nothing.
  *
  * Every operation returns a new pattern. This is not a stylistic preference:
  * a crease, once made, is a permanent change to the material. A crease
  * pattern is the whole history of a sheet, and history does not mutate.
  */
final case class CreasePattern private (paper: Polygon, creases: Vector[Crease]):

  /** Add the part of an infinite fold line that falls on the paper. */
  def fold(l: Line, assignment: Assignment)(using tol: Tol): CreasePattern =
    paper.clip(l).foldLeft(this)((cp, s) => cp.addSeg(s, assignment))

  def fold(ls: IterableOnce[Line], assignment: Assignment)(using Tol): CreasePattern =
    ls.iterator.foldLeft(this)((cp, l) => cp.fold(l, assignment))

  /** Add a crease given by two points, trimmed to the paper. */
  def crease(a: Pt, b: Pt, assignment: Assignment)(using tol: Tol): CreasePattern =
    if a ~= b then this else addSeg(Seg(a, b), assignment)

  def crease(s: Seg, assignment: Assignment)(using Tol): CreasePattern =
    crease(s.a, s.b, assignment)

  def mountain(a: Pt, b: Pt)(using Tol): CreasePattern = crease(a, b, Assignment.Mountain)
  def valley(a: Pt, b: Pt)(using Tol): CreasePattern = crease(a, b, Assignment.Valley)
  def unassigned(a: Pt, b: Pt)(using Tol): CreasePattern = crease(a, b, Assignment.Unassigned)

  def map(f: Crease => Crease): CreasePattern = CreasePattern(paper, creases.map(f))

  /** Turn the sheet over: every mountain becomes a valley and vice versa. */
  def flipOver: CreasePattern = map(c => c.withAssignment(c.assignment.opposite))

  def transform(f: Pt => Pt): CreasePattern =
    CreasePattern(Polygon(paper.vertices.map(f)), creases.map(c => Crease(Seg(f(c.a), f(c.b)), c.assignment)))

  /** Reflect the whole pattern across a line -- the closest this model comes to
    * actually folding. */
  def reflect(l: Line): CreasePattern = transform(l.reflect)

  def points: Vector[Pt] = creases.flatMap(c => Vector(c.a, c.b))

  def planarize(using Tol): PlanarGraph = Planarize(this)

  /** Every crease already on the paper is trimmed, so this only guards the
    * degenerate case and clips to the sheet. */
  private def addSeg(s: Seg, assignment: Assignment)(using tol: Tol): CreasePattern =
    if s.isDegenerate then this
    else
      val kept =
        if paper.contains(s.a) && paper.contains(s.b) then Vector(s)
        else paper.clip(s.line).flatMap(clipped => overlap(s, clipped))
      CreasePattern(paper, creases ++ kept.map(Crease(_, assignment)))

  private def overlap(s: Seg, other: Seg)(using tol: Tol): Option[Seg] =
    val l = s.line
    val (t0, t1) = (l.paramOf(s.a), l.paramOf(s.b))
    val (u0, u1) = (l.paramOf(other.a), l.paramOf(other.b))
    val lo = math.max(math.min(t0, t1), math.min(u0, u1))
    val hi = math.min(math.max(t0, t1), math.max(u0, u1))
    Option.when(hi - lo > tol.value)(Seg(l.at(lo), l.at(hi)))

object CreasePattern:
  /** The only way in: a blank sheet. */
  def blank(paper: Polygon): CreasePattern = CreasePattern(paper.ccw, Vector.empty)

  def square(side: Double = 200): CreasePattern = blank(Polygon.square(side))
