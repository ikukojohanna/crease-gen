package origami.pattern

import origami.geometry.{Line, Polygon, Pt, Seg, Tol}

/** Paper plus creases. Every crease is clipped to the paper. */
final case class CreasePattern private (paper: Polygon, creases: Vector[Crease]):

  def fold(l: Line, assignment: Assignment)(using tol: Tol): CreasePattern =
    paper.clip(l).foldLeft(this)((cp, s) => cp.addSeg(s, assignment))

  def fold(ls: IterableOnce[Line], assignment: Assignment)(using Tol): CreasePattern =
    ls.iterator.foldLeft(this)((cp, l) => cp.fold(l, assignment))

  def crease(a: Pt, b: Pt, assignment: Assignment)(using tol: Tol): CreasePattern =
    if a ~= b then this else addSeg(Seg(a, b), assignment)

  def crease(s: Seg, assignment: Assignment)(using Tol): CreasePattern =
    crease(s.a, s.b, assignment)

  def creaseAll(cs: IterableOnce[(Pt, Pt, Assignment)])(using Tol): CreasePattern =
    cs.iterator.foldLeft(this) { case (cp, (a, b, assignment)) => cp.crease(a, b, assignment) }

  def mountain(a: Pt, b: Pt)(using Tol): CreasePattern = crease(a, b, Assignment.Mountain)
  def valley(a: Pt, b: Pt)(using Tol): CreasePattern = crease(a, b, Assignment.Valley)
  def unassigned(a: Pt, b: Pt)(using Tol): CreasePattern = crease(a, b, Assignment.Unassigned)

  def map(f: Crease => Crease): CreasePattern = CreasePattern(paper, creases.map(f))

  def flipOver: CreasePattern = map(c => c.withAssignment(c.assignment.opposite))

  def transform(f: Pt => Pt): CreasePattern =
    CreasePattern(Polygon(paper.vertices.map(f)), creases.map(c => Crease(Seg(f(c.a), f(c.b)), c.assignment)))

  def reflect(l: Line): CreasePattern = transform(l.reflect)

  def points: Vector[Pt] = creases.flatMap(c => Vector(c.a, c.b))

  def planarize(using Tol): PlanarGraph = Planarize(this)

  private def addSeg(s: Seg, assignment: Assignment)(using tol: Tol): CreasePattern =
    if s.isDegenerate then this
    else
      val kept =
        if paper.contains(s.a) && paper.contains(s.b) then Vector(s)
        else paper.clip(s.line).flatMap(clipped => s.collinearOverlap(clipped, tol.value))
      CreasePattern(paper, creases ++ kept.map(Crease(_, assignment)))

object CreasePattern:
  def blank(paper: Polygon): CreasePattern = CreasePattern(paper.ccw, Vector.empty)

  def square(side: Double = 200): CreasePattern = blank(Polygon.square(side))
