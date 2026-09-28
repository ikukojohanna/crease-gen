package origami


/** A model: a name, the sheet it is cut from, and what we claim about it.
  *
  * `caveat` is the interesting field. It is where a model records that the
  * checker will pass it and the paper will not -- the gap between what the type
  * proves and what the world does.
  */
final case class Model(name: String, pattern: CreasePattern, note: String = "",
    caveat: Option[String] = None,
    /** Symmetries the labelling should respect, if any labelling can. */
    symmetries: Vector[Pt => Pt] = Vector.empty):
  def check(using Tol): Either[Vector[Violation], FlatFoldable] = FlatFoldable.from(pattern)

/** The library.
  *
  * Some of these are built by placing coordinates, some by folding: applying
  * axioms to references that already exist on the sheet. The second kind reads
  * like a set of instructions because that is exactly what it is.
  */
object Patterns:

  // ---------------------------------------------------------------- pleats --

  /** The simplest thing that folds: parallel creases, alternating. No interior
    * vertices at all, so the laws hold vacuously -- and indeed an accordion
    * folds no matter how you space it. */
  def accordion(n: Int = 8, size: Double = 200)(using Tol): Model =
    val cp = (1 until n).foldLeft(CreasePattern.square(size)): (p, i) =>
      val x = size * i / n
      p.crease(Pt(x, 0), Pt(x, size), if i % 2 == 1 then Assignment.Mountain else Assignment.Valley)
    Model("accordion", cp, s"$n panels; no interior vertices, so nothing to check")

  // ------------------------------------------------------------- miura-ori --

  /** Miura-ori. Every interior vertex is degree four, and degree-four vertices
    * are always three of one kind and one of the other. The whole sheet opens
    * and closes with a single degree of freedom. */
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

    // straight creases: one per interior row, alternating by row
    val withRows = (1 until rows).foldLeft(blank): (cp, j) =>
      cp.crease(p(0, j), p(cols, j), if j % 2 == 0 then Assignment.Valley else Assignment.Mountain)

    // Zigzag creases, alternating in *both* directions. Along a column that is
    // what Maekawa forces. Across a band it is what makes the band an accordion
    // rather than a roll: parallel creases that all fold the same way spiral
    // inwards instead of opening and closing, and a rolled Miura is not a
    // Miura. Both labellings satisfy every local law, and both fold flat to the
    // same outline -- the difference is the motion, and the motion is the point.
    val cp = (for i <- 1 until cols; j <- 0 until rows yield (i, j))
      .foldLeft(withRows): (cp, ij) =>
        val (i, j) = ij
        cp.crease(p(i, j), p(i, j + 1),
          if (i + j) % 2 == 0 then Assignment.Mountain else Assignment.Valley)

    Model("miura-ori", cp, s"$cols x $rows cells, rigid-foldable, one degree of freedom")

  // ------------------------------------------------------------ yoshimura --

  /** The diamond (Yoshimura) pattern: what a thin cylinder does when you crush
    * it. Interior vertices are degree six.
    *
    * The tempting labelling -- every horizontal a mountain, every diagonal a
    * valley -- satisfies Maekawa at every single vertex, four to two, and has
    * no valid stacking at all. The geometry is left unlabelled here so the
    * search can find one that actually folds.
    */
  def yoshimura(cols: Int = 4, rows: Int = 4, cell: Double = 48, height: Double = 36,
      uniform: Boolean = false)(using Tol): Model =
    val (w, h) = (cols * cell, rows * height)
    def offset(j: Int) = if j % 2 == 1 then cell / 2 else 0.0
    val ridge = if uniform then Assignment.Mountain else Assignment.Unassigned
    val slant = if uniform then Assignment.Valley else Assignment.Unassigned

    val withRows = (1 until rows).foldLeft(CreasePattern.blank(Polygon.rectangle(w, h))): (cp, j) =>
      cp.crease(Pt(0, j * height), Pt(w, j * height), ridge)

    val cp = (for j <- 0 until rows; i <- -1 to cols + 1; s <- Seq(-1, 1) yield (i, j, s))
      .foldLeft(withRows): (cp, ijs) =>
        val (i, j, s) = ijs
        val a = Pt(i * cell + offset(j), j * height)
        val b = Pt(i * cell + offset(j) + s * cell / 2, (j + 1) * height)
        cp.crease(a, b, slant)

    Model("yoshimura", cp, s"$cols x $rows diamonds, degree-six vertices",
      symmetries = Vector(Symmetry.halfTurn(Pt(w / 2, h / 2))))

  // ----------------------------------------------------------- classic bases --

  /** The waterbomb base, built by folding rather than by drawing.
    *
    * Note what Maekawa forces: of the four obvious lines on a square -- two
    * diagonals and two midlines -- only three can be folded. Eight creases at
    * the centre would need five of one kind and three of the other, and a
    * symmetric four-four labelling is simply not foldable. The fourth line is
    * still creased; it just stays flat. That is a constraint discovered by the
    * theorem, not by the diagram.
    */
  def waterbombBase(size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val cp = CreasePattern.square(size)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Mountain)
      .crease(Axioms.o2(a, b).get.let(l => clipToSquare(l, size)), Assignment.Valley)
      .crease(Axioms.o2(b, c).get.let(l => clipToSquare(l, size)), Assignment.Flat)
    Model("waterbomb-base", cp, "two diagonals folded, one midline folded, one midline left flat")

  /** The preliminary (square) base: the waterbomb turned inside out. */
  def preliminaryBase(size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val cp = CreasePattern.square(size)
      .crease(Axioms.o2(a, b).get.let(l => clipToSquare(l, size)), Assignment.Valley)
      .crease(Axioms.o2(b, c).get.let(l => clipToSquare(l, size)), Assignment.Valley)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Flat)
    Model("preliminary-base", cp, "two midlines folded, one diagonal folded, one diagonal left flat")

  /** The bird base, constructed with the axioms.
    *
    * The eight slanted creases are not drawn at 22.5 degrees; they are *found*,
    * by folding each edge of the square onto the diagonal next to it -- axiom
    * O3, the fold that bisects an angle. The kite points where they land are
    * intersections, not measurements. Nothing here knows about square roots of
    * two; the paper works that out.
    */
  def birdBase(size: Double = 200)(using tol: Tol): Model =
    val (a, b, c, d) = corners(size)
    val centre = Pt(size / 2, size / 2)
    val midV = Axioms.o2(a, b).get
    val midH = Axioms.o2(b, c).get

    // At each corner, fold the adjacent edge onto the diagonal (O3). O3 offers
    // two bisectors, internal and external -- the origamist takes the internal
    // one, so we ask for the bisector lying inside the corner's wedge.
    def internalBisector(corner: Pt, along: Pt, onto: Pt): Option[Line] =
      val wedge = ((along - corner).normalized + (onto - corner).normalized).normalized
      Axioms.o3(Line.through(corner, along), Line.through(corner, onto))
        .find(l => l.contains(corner) && math.abs(l.dir.dot(wedge)) > 1 - 1e-9)

    val slants = Vector(
      (a, c, Vector(b, d)), (b, d, Vector(a, c)),
      (c, a, Vector(b, d)), (d, b, Vector(a, c))
    ).flatMap((corner, far, ns) => ns.flatMap(n => internalBisector(corner, far, n).map((corner, _))))

    // Where each bisector first meets a midline: the kite points.
    val landings = slants.flatMap: (corner, l) =>
      Vector(midV, midH).flatMap(m => l.intersect(m)).filter(p => insideSquare(p, size))
        .sortBy(_.distTo(corner)).headOption.map(p => (corner, p))

    val kite = landings.map(_._2).foldLeft(Vector.empty[Pt])((acc, p) =>
      if acc.exists(_ ~= p) then acc else acc :+ p)

    val withRefs = CreasePattern.square(size)
      .crease(a, c, Assignment.Unassigned)
      .crease(b, d, Assignment.Unassigned)
      .crease(clipToSquare(midV, size), Assignment.Unassigned)
      .crease(clipToSquare(midH, size), Assignment.Unassigned)

    val withSlants = landings.foldLeft(withRefs)((cp, cl) => cp.crease(cl._1, cl._2, Assignment.Unassigned))

    // The inner square joins consecutive kite points.
    val sorted = kite.sortBy(p => (p - centre).angle)
    val cp = sorted.indices.foldLeft(withSlants): (p, i) =>
      p.crease(sorted(i), sorted((i + 1) % sorted.length), Assignment.Unassigned)

    Model("bird-base", cp,
      s"geometry from axioms O1/O2/O3; ${kite.length} kite points found by intersection, " +
        "labelling by symmetry-constrained search",
      symmetries = Vector(Symmetry.mirror(Line.through(a, c))))

  // --------------------------------------------------------- tessellations --

  /** Waterbomb tessellation: a square grid with a diagonal in every cell,
    * flipping direction row by row, giving degree-six vertices throughout. */
  def waterbombTessellation(cols: Int = 5, rows: Int = 5, cell: Double = 38,
      uniform: Boolean = false)(using Tol): Model =
    val (w, h) = (cols * cell, rows * cell)
    val gridKind = if uniform then Assignment.Mountain else Assignment.Unassigned
    val diagKind = if uniform then Assignment.Valley else Assignment.Unassigned
    val grid = CreasePattern.blank(Polygon.rectangle(w, h))
    val withCols = (1 until cols).foldLeft(grid)((cp, i) =>
      cp.crease(Pt(i * cell, 0), Pt(i * cell, h), gridKind))
    val withRows = (1 until rows).foldLeft(withCols)((cp, j) =>
      cp.crease(Pt(0, j * cell), Pt(w, j * cell), gridKind))
    val cp = (for i <- 0 until cols; j <- 0 until rows yield (i, j)).foldLeft(withRows): (cp, ij) =>
      val (i, j) = ij
      val (x, y) = (i * cell, j * cell)
      if j % 2 == 0 then cp.crease(Pt(x, y), Pt(x + cell, y + cell), diagKind)
      else cp.crease(Pt(x + cell, y), Pt(x, y + cell), diagKind)
    Model("waterbomb-tessellation", cp, s"$cols x $rows cells, degree-six vertices",
      symmetries = Vector(Symmetry.halfTurn(Pt(w / 2, h / 2))))

  /** One local rule, applied everywhere: grid lines mountain, diagonals valley.
    *
    * Every interior vertex satisfies Kawasaki, Maekawa and big-little-big --
    * four mountains to two valleys, exactly as the theorem asks, at both kinds
    * of vertex, at every size of sheet. And from four cells square upwards, the
    * layers have nowhere to go.
    *
    * This is the sharpest thing the program has to say, and the detail that
    * makes it sharp is the size. The rule folds on a two-by-two sheet. It folds
    * on a three-by-three. It fails on four-by-four and never works again. There
    * is no sheet small enough to test on and no vertex to point at: the
    * obstruction is not anywhere, it is everywhere at once.
    *
    * Checking a rule pointwise is not checking the rule.
    */
  def uniformRule(using Tol): Model =
    val base = waterbombTessellation(4, 4, 44, uniform = true)
    base.copy(name = "uniform-rule", symmetries = Vector.empty,
      note = "grid lines mountain, diagonals valley, applied everywhere",
      caveat = Some("every interior vertex passes every local law and no stacking exists. " +
        "The same rule folds fine at 3x3 -- the obstruction only appears at 4x4."))

  // -------------------------------------------------------- counterexamples --

  /** The hyperbolic paraboloid: concentric squares alternating mountain and
    * valley, plus both diagonals. The most-photographed decorative fold there
    * is -- and the checker rejects it.
    *
    * The rejection is correct. At every corner of every ring, four creases meet:
    * two ring edges of one kind and two diagonal halves of another. Two and two.
    * Maekawa needs three and one, and no labelling of a uniform ring can give
    * it. The hypar exists in paper because the paper *bends*: it is not a
    * folding of flat facets at all. Demaine, Demaine, Hart, Price and Tachi
    * proved exactly this in 2011.
    *
    * A checker that passed this pattern would be lying to you.
    */
  def hypar(rings: Int = 7, size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val withDiagonals = CreasePattern.square(size)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Mountain)
    val cp = (1 until rings).foldLeft(withDiagonals): (cp, k) =>
      val t = size / 2 * k / rings
      val ring = Vector(Pt(t, t), Pt(size - t, t), Pt(size - t, size - t), Pt(t, size - t))
      val kind = if k % 2 == 0 then Assignment.Mountain else Assignment.Valley
      ring.indices.foldLeft(cp)((p, i) => p.crease(ring(i), ring((i + 1) % 4), kind))
    Model("hypar", cp, s"$rings concentric rings plus both diagonals, labelled the classic way",
      caveat = Some("this rejection is the right answer. The hypar is foldable in paper but " +
        "not flat-foldable, and not a folding of flat facets at all -- the paper has to bend."))

  /** Both diagonals of a square, folded the same way. Four creases at the
    * centre can never satisfy |M - V| = 2. Try it with real paper. */
  def impossibleX(size: Double = 200)(using Tol): Model =
    val (a, b, c, d) = corners(size)
    val cp = CreasePattern.square(size)
      .crease(a, c, Assignment.Mountain)
      .crease(b, d, Assignment.Mountain)
    Model("impossible-x", cp, "Maekawa says no: four creases, four mountains")

  /** A crease that stops in the middle of the sheet. Paper does not tear. */
  def deadEnd(size: Double = 200)(using Tol): Model =
    val cp = CreasePattern.square(size)
      .crease(Pt(0, 0), Pt(size / 2, size / 2), Assignment.Valley)
    Model("dead-end", cp, "a crease ending in mid-sheet has nothing to fold against")

  def all(using Tol): Vector[Model] = Vector(
    accordion(), miura(), yoshimura(), waterbombBase(), preliminaryBase(),
    birdBase(), waterbombTessellation(), hypar(), uniformRule, impossibleX(), deadEnd())

  // ------------------------------------------------------------- utilities --

  private def corners(size: Double): (Pt, Pt, Pt, Pt) =
    (Pt(0, 0), Pt(size, 0), Pt(size, size), Pt(0, size))

  private def insideSquare(p: Pt, size: Double)(using tol: Tol): Boolean =
    p.x >= -tol.value && p.x <= size + tol.value && p.y >= -tol.value && p.y <= size + tol.value

  private def clipToSquare(l: Line, size: Double)(using Tol): Seg =
    Polygon.square(size).clip(l).head

  extension [A](a: A) private def let[B](f: A => B): B = f(a)
