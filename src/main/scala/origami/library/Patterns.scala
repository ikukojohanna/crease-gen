package origami.library

import origami.geometry.{Axioms, Line, Polygon, Pt, Tol}
import origami.laws.Symmetry
import origami.pattern.{Assignment, CreasePattern}
import origami.utils.SeqUtils.*

object Patterns:

  def accordion(n: Int = 8, size: Double = 200)(using Tol): Model =
    val cp = CreasePattern.square(size).creaseAll:
      (1 until n).map: i =>
        val x = size * i / n
        (Pt(x, 0), Pt(x, size), Assignment.alternating(i))
    Model("accordion", cp, s"$n panels; no interior vertices, so nothing to check")

  def miura(cols: Int = 6, rows: Int = 4, cell: Double = 34, height: Double = 30,
      shift: Double = 12)(using Tol): Model =
    def offset(j: Int) = if j % 2 == 1 then shift else 0.0
    def p(i: Int, j: Int) = Pt(i * cell + offset(j), j * height)

    val outline =
      (0 to cols).map(p(_, 0)).toVector ++
        (1 to rows).map(p(cols, _)).toVector ++
        (0 to cols).reverse.map(p(_, rows)).toVector ++
        (rows - 1 to 1 by -1).map(p(0, _)).toVector

    val blank = CreasePattern.blank(Polygon(outline))

    val straights = (1 until rows).map(j => (p(0, j), p(cols, j), Assignment.alternating(j)))

    // Alternate both ways, or the bands roll instead of pleating.
    val zigzags = for i <- 1 until cols; j <- 0 until rows yield
      (p(i, j), p(i, j + 1), Assignment.alternating(i + j, onEven = Assignment.Mountain))

    Model("miura-ori", blank.creaseAll(straights).creaseAll(zigzags), s"$cols x $rows cells, rigid-foldable, one degree of freedom")

  /** Unlabelled: the obvious labelling has no valid stacking. */
  def yoshimura(cols: Int = 4, rows: Int = 4, cell: Double = 48, height: Double = 36,
      uniform: Boolean = false)(using Tol): Model =
    val (w, h) = (cols * cell, rows * height)
    def offset(j: Int) = if j % 2 == 1 then cell / 2 else 0.0
    val ridge = if uniform then Assignment.Mountain else Assignment.Unassigned
    val slant = if uniform then Assignment.Valley else Assignment.Unassigned

    val ridges = (1 until rows).map(j => (Pt(0, j * height), Pt(w, j * height), ridge))

    val slants = for j <- 0 until rows; i <- -1 to cols + 1; s <- Seq(-1, 1) yield
      (Pt(i * cell + offset(j), j * height), Pt(i * cell + offset(j) + s * cell / 2, (j + 1) * height), slant)

    val cp = CreasePattern.blank(Polygon.rectangle(w, h)).creaseAll(ridges).creaseAll(slants)
    Model("yoshimura", cp, s"$cols x $rows diamonds, degree-six vertices",
      symmetries = Vector(Symmetry.halfTurn(Pt(w / 2, h / 2))))

  /** Maekawa allows only three of the four lines to fold; the fourth stays flat. */
  def waterbombBase(size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val cp = CreasePattern.square(size)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Mountain)
      .fold(Axioms.o2(a, b).get, Assignment.Valley)
      .fold(Axioms.o2(b, c).get, Assignment.Flat)
    Model("waterbomb-base", cp, "two diagonals folded, one midline folded, one midline left flat")

  def preliminaryBase(size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val cp = CreasePattern.square(size)
      .fold(Axioms.o2(a, b).get, Assignment.Valley)
      .fold(Axioms.o2(b, c).get, Assignment.Valley)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Flat)
    Model("preliminary-base", cp, "two midlines folded, one diagonal folded, one diagonal left flat")

  def birdBase(size: Double = 200)(using tol: Tol): Model =
    val (a, b, c, d) = corners(size)
    val centre = Pt(size / 2, size / 2)
    val midV = Axioms.o2(a, b).get
    val midH = Axioms.o2(b, c).get

    /** Fold the edge `corner`-`next` onto the diagonal `corner`-`opposite` (O3), keeping the fold inside the corner. */
    def edgeOntoDiagonal(corner: Pt, opposite: Pt, next: Pt): Option[Line] =
      val wedge = ((opposite - corner).normalized + (next - corner).normalized).normalized
      Axioms.o3(Line.through(corner, opposite), Line.through(corner, next))
        .find(l => l.contains(corner) && math.abs(l.dir.dot(wedge)) > 1 - 1e-9)

    def firstMidlineHit(corner: Pt, slant: Line): Option[Pt] =
      Vector(midV, midH).flatMap(slant.intersect).filter(insideSquare(_, size)).minByOption(_.distTo(corner))

    // Each corner, the corner opposite it, and the two corners next to it.
    val neighbourhoods = Vector((a, c, Vector(b, d)), (b, d, Vector(a, c)), (c, a, Vector(b, d)), (d, b, Vector(a, c)))
    val slants =
      for
        (corner, opposite, adjacent) <- neighbourhoods
        next <- adjacent
        fold <- edgeOntoDiagonal(corner, opposite, next)
        kitePoint <- firstMidlineHit(corner, fold)
      yield (corner, kitePoint)

    val kite = slants.map(_._2).distinctWith(_ ~= _)
    val inner = kite.sortBy(p => (p - centre).angle).cyclicPairs

    val cp = CreasePattern.square(size)
      .crease(a, c, Assignment.Unassigned)
      .crease(b, d, Assignment.Unassigned)
      .fold(midV, Assignment.Unassigned)
      .fold(midH, Assignment.Unassigned)
      .creaseAll(slants.map((corner, kitePoint) => (corner, kitePoint, Assignment.Unassigned)))
      .creaseAll(inner.map((p, q) => (p, q, Assignment.Unassigned)))

    Model("bird-base", cp,
      s"geometry from axioms O1/O2/O3; ${kite.length} kite points found by intersection, " +
        "labelling by symmetry-constrained search",
      symmetries = Vector(Symmetry.mirror(Line.through(a, c))))

  def waterbombTessellation(cols: Int = 5, rows: Int = 5, cell: Double = 38,
      uniform: Boolean = false)(using Tol): Model =
    val (w, h) = (cols * cell, rows * cell)
    val gridKind = if uniform then Assignment.Mountain else Assignment.Unassigned
    val diagKind = if uniform then Assignment.Valley else Assignment.Unassigned
    val verticals = (1 until cols).map(i => (Pt(i * cell, 0), Pt(i * cell, h), gridKind))
    val horizontals = (1 until rows).map(j => (Pt(0, j * cell), Pt(w, j * cell), gridKind))
    val diagonals = for i <- 0 until cols; j <- 0 until rows yield
      val (x, y) = (i * cell, j * cell)
      if j % 2 == 0 then (Pt(x, y), Pt(x + cell, y + cell), diagKind)
      else (Pt(x + cell, y), Pt(x, y + cell), diagKind)
    val cp = CreasePattern.blank(Polygon.rectangle(w, h))
      .creaseAll(verticals)
      .creaseAll(horizontals)
      .creaseAll(diagonals)
    Model("waterbomb-tessellation", cp, s"$cols x $rows cells, degree-six vertices",
      symmetries = Vector(Symmetry.halfTurn(Pt(w / 2, h / 2))))

  def uniformRule(using Tol): Model =
    val base = waterbombTessellation(4, 4, 44, uniform = true)
    base.copy(name = "uniform-rule", symmetries = Vector.empty,
      note = "grid lines mountain, diagonals valley, applied everywhere",
      caveat = Some("every interior vertex passes every local law and no stacking exists. " +
        "The same rule folds fine at 3x3 -- the obstruction only appears at 4x4."))

  def hypar(rings: Int = 7, size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val squares = (1 until rings).flatMap: k =>
      val t = size / 2 * k / rings
      val ring = Vector(Pt(t, t), Pt(size - t, t), Pt(size - t, size - t), Pt(t, size - t))
      val kind = Assignment.alternating(k, onEven = Assignment.Mountain)
      ring.cyclicPairs.map((p, q) => (p, q, kind))
    val cp = CreasePattern.square(size)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Mountain)
      .creaseAll(squares)
    Model("hypar", cp, s"$rings concentric rings plus both diagonals, labelled the classic way",
      caveat = Some("this rejection is the right answer. The hypar is foldable in paper but " +
        "not flat-foldable, and not a folding of flat facets at all -- the paper has to bend."))

  def impossibleX(size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val cp = CreasePattern.square(size)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Mountain)
    Model("impossible-x", cp, "Maekawa says no: four creases, four mountains")

  def deadEnd(size: Double = 200)(using Tol): Model =
    val cp = CreasePattern.square(size)
      .crease(Pt(0, 0), Pt(size / 2, size / 2), Assignment.Valley)
    Model("dead-end", cp, "a crease ending in mid-sheet has nothing to fold against")

  def all(using Tol): Vector[Model] = Vector(
    accordion(), miura(), yoshimura(), waterbombBase(), preliminaryBase(),
    birdBase(), waterbombTessellation(), hypar(), uniformRule, impossibleX(), deadEnd())

  private def corners(size: Double): (Pt, Pt, Pt, Pt) =
    (Pt(0, 0), Pt(size, 0), Pt(size, size), Pt(0, size))

  private def insideSquare(p: Pt, size: Double)(using tol: Tol): Boolean =
    p.x >= -tol.value && p.x <= size + tol.value && p.y >= -tol.value && p.y <= size + tol.value
