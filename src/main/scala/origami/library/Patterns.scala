package origami.library

import origami.geometry.{Axioms, Geometry, Line, Polygon, Pt, Tol}
import origami.laws.Symmetry
import origami.pattern.{Assignment, CreasePattern}
import origami.tiling.{Graded, ShrinkRotate, Tiling, Tilings}
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
    if uniform then Model("yoshimura", cp, s"$cols x $rows diamonds, ridges mountain, slants valley")
    else Model("yoshimura-lines", cp, s"the Yoshimura's lines, $cols x $rows diamonds, every one unassigned",
      caveat = Some(searchedCaveat("Yoshimura")),
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

  /** The bird base: geometry from the axioms, the textbook mountain-valley assignment. */
  def birdBase(size: Double = 200)(using Tol): Model =
    val g = birdGeometry(size)
    // The crease lines alone don't determine the model: the same lines fold flat in other ways
    // too (see `birdLines`), so the bird base is defined by its assignment, not found by search.
    val cp = CreasePattern.square(size)
      .crease(g.a, g.c, Assignment.Valley) // the spine
      .crease(g.b, g.d, Assignment.Flat) // precreased, unfolded in the base
      .creaseAll(g.edgeMids.map(m => (m, g.kite.minBy(_.distTo(m)), Assignment.Valley)))
      .creaseAll(g.kite.map(k => (k, g.centre, Assignment.Mountain)))
      .creaseAll(g.slants.map((corner, kitePoint) => (corner, kitePoint, Assignment.Mountain)))
      .creaseAll(g.inner.map((p, q) => (p, q, Assignment.Flat))) // petal-fold precreases
    Model("bird-base", cp,
      s"geometry from axioms O1/O2/O3; ${g.kite.length} kite points found by intersection, " +
        "textbook mountain-valley assignment")

  /** The bird base's crease lines with no assignment. The diagonals and the inner square may also stay flat. */
  def birdLines(size: Double = 200)(using Tol): Model =
    val g = birdGeometry(size)
    val cp = CreasePattern.square(size)
      .crease(g.a, g.c, Assignment.Optional)
      .crease(g.b, g.d, Assignment.Optional)
      .fold(g.midV, Assignment.Unassigned)
      .fold(g.midH, Assignment.Unassigned)
      .creaseAll(g.slants.map((corner, kitePoint) => (corner, kitePoint, Assignment.Unassigned)))
      .creaseAll(g.inner.map((p, q) => (p, q, Assignment.Optional)))
    Model("bird-lines", cp, "the bird base's lines, unassigned; diagonals and inner square may stay flat",
      caveat = Some(searchedCaveat("bird base")),
      symmetries = Vector(Symmetry.mirror(Line.through(g.a, g.c))))

  private final case class BirdGeometry(a: Pt, b: Pt, c: Pt, d: Pt, centre: Pt, midV: Line, midH: Line,
      edgeMids: Vector[Pt], slants: Vector[(Pt, Pt)], kite: Vector[Pt], inner: Vector[(Pt, Pt)])

  private def birdGeometry(size: Double)(using tol: Tol): BirdGeometry =
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

    // Where each midline meets the edge of the sheet.
    val edgeMids = Vector(midV, midH).flatMap(l => Polygon.square(size).clip(l)).flatMap(s => Vector(s.a, s.b))
    BirdGeometry(a, b, c, d, centre, midV, midH, edgeMids, slants, kite, inner)

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
    if uniform then Model("waterbomb-tessellation", cp, s"$cols x $rows cells, grid mountain, diagonals valley")
    else Model("waterbomb-lines", cp, s"the waterbomb tessellation's lines, $cols x $rows cells, every one unassigned",
      caveat = Some(searchedCaveat("waterbomb tessellation")),
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

  def hexTwists(size: Int = 2, s: Double = 0.5, twist: Double = 30)(using Tol): Model =
    twists("hex-twists", Tilings.hexagonal(size), s"6.6.6 tiling, size $size", s, twist)

  def squareTwists(size: Int = 2, s: Double = 0.5, twist: Double = 30)(using Tol): Model =
    twists("square-twists", Tilings.square(size), s"4.4.4.4 tiling, size $size", s, twist)

  def triangleTwists(size: Int = 2, s: Double = 0.5, twist: Double = 30)(using Tol): Model =
    twists("triangle-twists", Tilings.triangular(size), s"3.3.3.3.3.3 tiling, size $size", s, twist)

  def trihexTwists(size: Int = 2, s: Double = 0.5, twist: Double = 30)(using Tol): Model =
    twists("trihex-twists", Tilings.trihexagonal(size), s"3.6.3.6 tiling, size $size", s, twist)

  /** Voronoi cells growing denser towards a focus, refined `depth` times, pleats kept at least `minWidth` mm. */
  def voronoiTwists(params: Graded.Params = Graded.Params(paperRadius = 100, spacing = 30, focus = Pt(15, 10),
      focusRadius = 60), s: Double = 0.5, twist: Double = 30, minWidth: Double = 2)(using Tol): Model =
    val pruned = Graded.pruned(params, s, Geometry.degrees(twist), minWidth)
    val model = twists("voronoi-twists", pruned.tiling, s"Voronoi refined ${params.depth} times towards a focus", s, twist)
    model.copy(note = model.note + s"; ${pruned.dropped.length} point(s) dropped for pleats under $minWidth mm")

  def all(using Tol): Vector[Model] = Vector(
    accordion(), miura(), yoshimura(), waterbombBase(), preliminaryBase(),
    birdBase(), birdLines(), waterbombTessellation(), hypar(), uniformRule, impossibleX(), deadEnd(),
    hexTwists(), squareTwists(), triangleTwists(), trihexTwists(), voronoiTwists())

  /** A shrink-and-rotate tessellation; the parameters here are fixed, so a rejection is a bug. */
  private def twists(name: String, tiling: Tiling, what: String, s: Double, twist: Double)(using Tol): Model =
    val cp = ShrinkRotate(tiling, s, Geometry.degrees(twist)).fold(p => throw IllegalArgumentException(p.explain), identity)
    Model(name, cp, f"$what, shrunk to $s%.2f and turned $twist%.0f deg; labelled by construction")

  private def searchedCaveat(model: String): String =
    s"these are flat foldings of the $model's lines, found by search, not the $model itself."

  private def corners(size: Double): (Pt, Pt, Pt, Pt) =
    (Pt(0, 0), Pt(size, 0), Pt(size, size), Pt(0, size))

  private def insideSquare(p: Pt, size: Double)(using tol: Tol): Boolean =
    p.x >= -tol.value && p.x <= size + tol.value && p.y >= -tol.value && p.y <= size + tol.value
