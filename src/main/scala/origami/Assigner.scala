package origami

import scala.collection.mutable

/** Given a figure the geometry allows, find mountains and valleys that work.
  *
  * Kawasaki is a property of the drawing; Maekawa and big-little-big constrain
  * the labelling. So labelling is a search problem, and a small one:
  * depth-first over the undecided creases, pruning a branch the moment a vertex
  * can no longer reach |M - V| = 2.
  *
  * Creases already marked Mountain, Valley or Flat are respected, so you can
  * pin the folds you know and let the solver finish the sheet.
  */
object Assigner:

  /** Any valid labelling. */
  def solve(g: PlanarGraph)(using Tol): Option[PlanarGraph] =
    run(g, identityGroups(g)).map(k => g.withAssignments((i, _) => k(i)))

  /** A valid labelling that respects a symmetry of the sheet.
    *
    * If the paper is symmetric, insisting the folds are too collapses the
    * search space and picks out the labelling an origamist would actually
    * draw. Creases that map to each other must be labelled the same, so the
    * unknowns are orbits rather than edges.
    */
  def solveSymmetric(g: PlanarGraph, symmetries: Vector[Pt => Pt])(using Tol): Option[PlanarGraph] =
    symmetryGroups(g, symmetries).flatMap(grp => run(g, grp)).map(k => g.withAssignments((i, _) => k(i)))

  /** Symmetric if possible, any labelling otherwise. */
  def solvePreferSymmetric(g: PlanarGraph, symmetries: Vector[Pt => Pt])(using Tol): Option[PlanarGraph] =
    solveSymmetric(g, symmetries).orElse(solve(g))

  /** How many ways can this figure be labelled?
    *
    * Kawasaki says a figure is foldable; it does not say it is foldable in only
    * one way. The count measures the freedom the geometry leaves -- and is an
    * upper bound, because passing the local laws is not the same as folding.
    */
  def countSolutions(g: PlanarGraph, limit: Int = 10000)(using Tol): Int =
    var found = 0
    forEachSolution(g, identityGroups(g), limit) { _ => found += 1; false }
    found

  /** Walk the labellings, stopping at the first the caller accepts.
    *
    * The point of streaming them rather than collecting them: there are usually
    * far too many to hold, and the caller is looking for one that passes a test
    * the local laws cannot express.
    */
  def findSolution(g: PlanarGraph, limit: Int = 100000, symmetries: Vector[Pt => Pt] = Vector.empty)
      (accept: PlanarGraph => Boolean)(using Tol): Option[(PlanarGraph, Int)] =
    val groups =
      if symmetries.isEmpty then identityGroups(g)
      else symmetryGroups(g, symmetries).getOrElse(identityGroups(g))
    var answer: Option[(PlanarGraph, Int)] = None
    var tried = 0
    forEachSolution(g, groups, limit): k =>
      val kinds = k.clone()
      val candidate = g.withAssignments((i, _) => kinds(i))
      tried += 1
      if accept(candidate) then { answer = Some((candidate, tried)); true } else false
    answer

  /** Up to `limit` labellings that satisfy the local laws, in search order. */
  def solutions(g: PlanarGraph, limit: Int = 500)(using Tol): Vector[PlanarGraph] =
    val out = scala.collection.mutable.ArrayBuffer.empty[PlanarGraph]
    forEachSolution(g, identityGroups(g), limit): k =>
      val kinds = k.clone()
      out += g.withAssignments((i, _) => kinds(i))
      false
    out.toVector

  /** Forget every mountain and valley, keeping only the geometry. */
  def blank(g: PlanarGraph): PlanarGraph =
    g.withAssignments((_, e) => if e.assignment == Assignment.Boundary then e.assignment
      else if e.assignment == Assignment.Flat then e.assignment
      else Assignment.Unassigned)

  def solveFlatFoldable(cp: CreasePattern)(using Tol): Either[Vector[Violation], FlatFoldable] =
    val g = cp.planarize
    solve(g) match
      case Some(solved) => FlatFoldable.from(solved)
      case None =>
        val geom = Laws.checkGeometry(g)
        Left(if geom.nonEmpty then geom else Laws.checkAll(g))

  // --------------------------------------------------------------- internals

  /** `groups(e)` is the unknown that decides edge `e`, or -1 if it is fixed. */
  private type Groups = Array[Int]

  private def identityGroups(g: PlanarGraph): Groups =
    var next = 0
    g.edges.map { e =>
      if e.assignment == Assignment.Unassigned then { next += 1; next - 1 } else -1
    }.toArray

  /** Orbits of the undecided edges under the given point symmetries. */
  private def symmetryGroups(g: PlanarGraph, symmetries: Vector[Pt => Pt])(using tol: Tol): Option[Groups] =
    val index = g.vertices.zipWithIndex
    def nearest(p: Pt): Option[Int] = index.find((q, _) => q ~= p).map(_._2)
    val edgeAt = g.edges.zipWithIndex.map((e, i) => (e.key, i)).toMap

    val parent = Array.tabulate(g.edges.length)(identity)
    def find(i: Int): Int = if parent(i) == i then i else { parent(i) = find(parent(i)); parent(i) }
    def union(a: Int, b: Int): Unit =
      val (ra, rb) = (find(a), find(b))
      if ra != rb then parent(ra) = rb

    val mapped = g.edges.zipWithIndex.forall: (e, i) =>
      symmetries.forall: sym =>
        (nearest(sym(g.vertices(e.u))), nearest(sym(g.vertices(e.v)))) match
          case (Some(u), Some(v)) =>
            val k = if u < v then (u, v) else (v, u)
            edgeAt.get(k) match
              case Some(j) if g.edges(j).assignment == e.assignment => union(i, j); true
              case _ => false
          case _ => false

    Option.when(mapped):
      var next = 0
      val label = mutable.HashMap.empty[Int, Int]
      g.edges.zipWithIndex.map { (e, i) =>
        if e.assignment != Assignment.Unassigned then -1
        else label.getOrElseUpdate(find(i), { next += 1; next - 1 })
      }.toArray

  private def run(g: PlanarGraph, groups: Groups)(using Tol): Option[Array[Assignment]] =
    var answer: Option[Array[Assignment]] = None
    forEachSolution(g, groups, 1) { k => answer = Some(k.clone()); true }
    answer

  /** Depth-first over the unknowns, checking every vertex the moment it is
    * fully decided and pruning as soon as |M - V| = 2 is out of reach. */
  /** Creases that are parallel and next to each other.
    *
    * Needed only as a hint, but an important one. The laws are indifferent
    * between a pleat and a roll -- parallel creases that all fold the same way
    * satisfy Maekawa exactly as well as ones that alternate, and fold flat to
    * the same outline -- so a search that always tries Mountain first quietly
    * returns rolls, which spiral shut instead of opening and closing. Knowing
    * which creases are neighbours lets the search prefer to alternate.
    */
  private def parallelNeighbours(g: PlanarGraph)(using tol: Tol): Array[Array[Int]] =
    val out = Array.fill(g.edges.length)(Array.empty[Int])
    val families = g.edges.indices.filterNot(e => g.edges(e).assignment == Assignment.Boundary)
      .groupBy: e =>
        val d = (g.vertices(g.edges(e).v) - g.vertices(g.edges(e).u)).normalized
        val a = Geometry.norm2Pi(math.atan2(d.y, d.x))
        math.round((if a >= math.Pi then a - math.Pi else a) / 1e-4) // direction, mod a half turn
    for (_, es) <- families do
      def offset(e: Int): Double =
        val u = g.vertices(g.edges(e).u)
        val d = (g.vertices(g.edges(e).v) - u).normalized
        u.x * -d.y + u.y * d.x
      val sorted = es.sortBy(offset).toArray
      // Link each crease to the nearest ones lying on a different parallel line.
      for i <- sorted.indices do
        val here = offset(sorted(i))
        val before = (i - 1 to 0 by -1).find(k => !tol.eqv(offset(sorted(k)), here)).map(sorted)
        val after = (i + 1 until sorted.length).find(k => !tol.eqv(offset(sorted(k)), here)).map(sorted)
        out(sorted(i)) = (before.toArray ++ after.toArray)
    out

  private def forEachSolution(g: PlanarGraph, groups: Groups, limit: Int)
      (onSolution: Array[Assignment] => Boolean)(using tol: Tol): Unit =
    val infos = g.interiorVertices
    val kinds = g.edges.map(_.assignment).toArray
    val nGroups = if groups.isEmpty then 0 else groups.max + 1
    val members = Array.fill(nGroups)(mutable.ArrayBuffer.empty[Int])
    groups.zipWithIndex.foreach((grp, e) => if grp >= 0 then members(grp) += e)

    val vertexEdges = infos.map(_.foldedEdges).toArray
    val groupVertices = Array.fill(nGroups)(mutable.Set.empty[Int])
    infos.zipWithIndex.foreach: (v, vi) =>
      v.foldedEdges.foreach(e => if groups(e) >= 0 then groupVertices(groups(e)) += vi)

    // Decide the most entangled unknowns first.
    val order = (0 until nGroups).sortBy(grp => -groupVertices(grp).size).toArray
    val neighbours = parallelNeighbours(g)

    /** Try the assignment that disagrees with the decided neighbours first, so
      * a pleat is found before a roll. Only the order of the two branches: both
      * are still explored, and nothing valid is ruled out. */
    def branches(grp: Int): List[Assignment] =
      var agreeM, agreeV = 0
      for e <- members(grp); p <- neighbours(e) do
        kinds(p) match
          case Assignment.Mountain => agreeM += 1
          case Assignment.Valley   => agreeV += 1
          case _                   => ()
      if agreeM > agreeV then List(Assignment.Valley, Assignment.Mountain)
      else if agreeV > agreeM then Assignment.mv
      else Assignment.mv

    def feasible(vi: Int): Boolean =
      var m, v, r = 0
      vertexEdges(vi).foreach: e =>
        kinds(e) match
          case Assignment.Mountain => m += 1
          case Assignment.Valley   => v += 1
          case _                   => r += 1
      if r > 0 then (0 to r).exists(k => math.abs((m + k) - (v + r - k)) == 2)
      else math.abs(m - v) == 2 && bigLittleBigOk(vi)

    def bigLittleBigOk(vi: Int): Boolean =
      val info = infos(vi)
      val s = info.sectors
      val n = s.length
      n < 4 || (0 until n).forall: i =>
        val (prev, here, next) = (s((i + n - 1) % n), s(i), s((i + 1) % n))
        !(tol.lt(here, prev) && tol.lt(here, next)) ||
          kinds(info.foldedEdges(i)) != kinds(info.foldedEdges((i + 1) % n))

    var found = 0
    var stop = false

    def go(k: Int): Unit =
      if stop || found >= limit then ()
      else if k == order.length then
        if infos.indices.forall(feasible) then
          found += 1
          if onSolution(kinds) then stop = true
      else
        val grp = order(k)
        branches(grp).foreach: choice =>
          if !stop && found < limit then
            members(grp).foreach(e => kinds(e) = choice)
            if groupVertices(grp).forall(feasible) then go(k + 1)
            if !stop then members(grp).foreach(e => kinds(e) = Assignment.Unassigned)

    if nGroups == 0 then
      if infos.indices.forall(feasible) then { found = 1; onSolution(kinds) }
    else go(0)

/** Reflections and rotations of a sheet, as plain point functions. */
object Symmetry:
  def mirror(l: Line): Pt => Pt = l.reflect
  def rotation(centre: Pt, turns: Int): Pt => Pt = p =>
    centre + (p - centre).rotate(2 * math.Pi / turns)
  def halfTurn(centre: Pt): Pt => Pt = rotation(centre, 2)
