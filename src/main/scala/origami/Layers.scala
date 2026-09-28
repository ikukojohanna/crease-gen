package origami

import scala.collection.mutable

/** The stacking order of the facets, bottom to top. */
final case class Stacking(order: Vector[Int]):
  lazy val layerOf: Map[Int, Int] = order.zipWithIndex.toMap
  def depth: Int = order.length

enum LayerVerdict:
  /** A stacking exists, and here it is. */
  case Stacked(stacking: Stacking, overlaps: Int, constraints: Int)
  /** No stacking exists. Every vertex obeys the local laws and the sheet still
    * cannot be folded: the layers would have to pass through each other. */
  case Impossible(overlaps: Int, constraints: Int)
  /** The search ran out of budget. The problem is NP-hard, so this is an answer
    * you have to be willing to give. */
  case GaveUp(steps: Int, overlaps: Int, constraints: Int)

  def ok: Boolean = this.isInstanceOf[LayerVerdict.Stacked]

  def describe: String = this match
    case Stacked(s, o, c) => s"stacks in ${s.depth} layers ($o overlapping pairs, $c constraints)"
    case Impossible(o, c) => s"no stacking exists ($o overlapping pairs, $c constraints)"
    case GaveUp(n, o, c)  => s"gave up after $n steps ($o overlapping pairs, $c constraints)"

/** Which facet lies on top of which.
  *
  * This is the part the local laws cannot reach. Kawasaki and Maekawa are
  * conditions at a vertex; they know nothing about two pieces of paper on
  * opposite sides of the sheet that happen to land on the same spot. Deciding
  * whether a consistent stacking exists is global, and Bern and Hayes showed in
  * 1996 that it is NP-hard -- so unlike everything else in this program, this is
  * a search that can legitimately give up.
  *
  * Justin's conditions say when a stacking is physically possible. Each is the
  * same sentence in a different costume: *paper does not pass through paper.*
  *
  *   - **taco-taco.** Two folds whose creases land on the same line. Each is a
  *     taco of two facets, closed at the crease. One taco may sit entirely
  *     inside the other, or entirely outside it, but the two may not interleave
  *     -- a shell that went in one side and out the other would have to cut
  *     through the closed end.
  *   - **taco-tortilla.** A fold whose crease lands inside a flat facet. That
  *     facet cannot slip between the two halves of the taco, for the same
  *     reason: the fold is closed.
  *   - **tortilla-tortilla.** Two facets joined along an *unfolded* crease are
  *     one continuous flat sheet, so nothing can be above one and below the
  *     other where they meet.
  *
  * The last two are a single statement -- a crease passing through a facet
  * pins that facet to one side of the crease's two facets -- and this code
  * treats them as one, with a flag for each so a talk can switch them off and
  * watch the checker start accepting the impossible.
  *
  * Taco-taco is subtler, and it is the condition that is easy to get wrong.
  * "The two tacos must not interleave" is not "neither facet of one lies
  * between the facets of the other": nesting is allowed, and nesting is what
  * happens every time you fold several layers at once. The correct statement is
  * a parity condition on four facets -- an even number of the four order
  * relations may be true -- and it is the only constraint here that is not a
  * simple equality.
  */
object Layers:

  /** Which of Justin's conditions to enforce. */
  final case class Conditions(tacoTaco: Boolean = true, tacoTortilla: Boolean = true,
      tortillaTortilla: Boolean = true)

  def solve(state: FoldedState, budget: Int = 400000,
      conditions: Conditions = Conditions())(using tol: Tol): LayerVerdict =
    val n = state.faces.facets.length
    if n == 0 then return LayerVerdict.Stacked(Stacking(Vector.empty), 0, 0)

    val folded = state.foldedFacets
    val tris = folded.map(Triangulate.apply)
    val eps = math.sqrt(math.max(1e-9, state.graph.paper.area)) * 1e-6

    // Which facets share area once folded. Facets that merely meet edge to edge
    // are touching, not overlapping, and have no order to argue about.
    val pairs = mutable.ArrayBuffer.empty[(Int, Int)]
    val overlaps = Array.fill(n)(mutable.BitSet.empty)
    for i <- 0 until n; j <- (i + 1) until n do
      if tris(i).exists(a => tris(j).exists(b => Overlap.triangles(a, b, eps))) then
        pairs += ((i, j))
        overlaps(i) += j
        overlaps(j) += i

    // A constraint: an even number of these "is above" relations may hold.
    val cons = mutable.ArrayBuffer.empty[Vector[(Int, Int)]]

    def defined(i: Int, j: Int): Boolean = i != j && overlaps(i)(j)

    /** Facets a and b end up on the same side of c. */
    def sameSide(a: Int, b: Int, c: Int): Unit =
      if a != b && defined(a, c) && defined(b, c) then cons += Vector((a, c), (b, c))

    /** The tacos {a,b} and {c,d} nest or miss, but do not interleave. */
    def nonCrossing(a: Int, b: Int, c: Int, d: Int): Unit =
      val lits = Vector((a, c), (b, c), (a, d), (b, d))
      if Set(a, b, c, d).size == 4 && lits.forall(defined) then cons += lits

    // Every crease, as it appears in the folded model, with the facets it joins.
    val creases = state.graph.edges.indices.toVector.flatMap: e =>
      val fs = state.faces.facesAt(e)
      val a = state.graph.edges(e).assignment
      Option.when((a.isFolded || a == Assignment.Flat) && fs.length == 2):
        Crossing(state.maps(fs(0))(state.faces.edgeSeg(e)), fs(0), fs(1), a.isFolded, a)

    // The order at a fold is not a choice. Valley-fold a face-up sheet and the
    // moving facet lands on top; mountain-fold it and the facet goes underneath;
    // turn the paper over and the two swap. Every folded crease therefore fixes
    // one relation outright, before the search begins.
    //
    // This is where Maekawa comes from. Nothing below mentions counting
    // mountains and valleys, but a vertex whose creases cannot satisfy
    // |M - V| = 2 produces a cycle of forced relations that no order can
    // satisfy, and the search rejects it on those grounds alone.
    val forced = creases.filter(_.folded).map: t =>
      val valley = t.assignment == Assignment.Valley
      val rightOnTop = if state.maps(t.left).facesUp then valley else !valley
      if rightOnTop then (t.right, t.left) else (t.left, t.right)

    // taco-taco
    if conditions.tacoTaco then
      for
        i <- creases.indices
        j <- (i + 1) until creases.length
        t1 = creases(i); t2 = creases(j)
        if t1.folded && t2.folded
        if t1.seg.line.sameAs(t2.seg.line) && collinearOverlap(t1.seg, t2.seg, eps)
        if openSameWay(t1, t2, folded)
      do nonCrossing(t1.left, t1.right, t2.left, t2.right)

    // taco-tortilla and tortilla-tortilla: a crease running through a facet
    // pins that facet to one side of the crease.
    for
      t <- creases
      if (if t.folded then conditions.tacoTortilla else conditions.tortillaTortilla)
      f <- 0 until n
      if f != t.left && f != t.right
      if tris(f).exists(tr => Overlap.segmentThrough(t.seg, tr, eps))
    do sameSide(t.left, t.right, f)

    val solver = Solver(n, cons.toVector, pairs.toVector, forced, budget)
    solver.run() match
      case Some(stacking)         => LayerVerdict.Stacked(stacking, pairs.length, cons.length)
      case None if solver.gaveUp  => LayerVerdict.GaveUp(solver.steps, pairs.length, cons.length)
      case None                   => LayerVerdict.Impossible(pairs.length, cons.length)

  /** How many sheets lie over the busiest point of the model. */
  def maxDepth(state: FoldedState, samples: Int = 160)(using Tol): Int =
    val tris = state.foldedFacets.map(Triangulate.apply)
    val (lo, hi) = state.bounds
    val eps = math.max(hi.x - lo.x, hi.y - lo.y) * 1e-9
    var best = 0
    for i <- 0 until samples; j <- 0 until samples do
      val p = Pt(lo.x + (hi.x - lo.x) * (i + 0.37) / samples,
        lo.y + (hi.y - lo.y) * (j + 0.61) / samples)
      val d = tris.count(_.exists(t => inside(p, t, eps)))
      if d > best then best = d
    best

  private final case class Crossing(seg: Seg, left: Int, right: Int, folded: Boolean,
      assignment: Assignment)

  /** Strictly inside, so a point on a shared edge is not counted twice. */
  private def inside(p: Pt, t: Tri, eps: Double): Boolean =
    t.edges.forall((a, b) => (p - a).cross(b - a) < -eps * (b - a).norm)

  private def collinearOverlap(s1: Seg, s2: Seg, eps: Double): Boolean =
    val l = s1.line
    val (a, b) = (l.paramOf(s1.a), l.paramOf(s1.b))
    val (c, d) = (l.paramOf(s2.a), l.paramOf(s2.b))
    math.min(math.max(a, b), math.max(c, d)) - math.max(math.min(a, b), math.min(c, d)) > eps

  /** Two folds on the same line constrain each other only if they open towards
    * the same side. Back to back, their shells never meet. */
  private def openSameWay(t1: Crossing, t2: Crossing, folded: Vector[Polygon]): Boolean =
    val l = t1.seg.line
    def side(f: Int): Double = math.signum(l.signedDist(folded(f).centroid))
    side(t1.left) == side(t1.right) && side(t2.left) == side(t2.right) &&
      side(t1.left) == side(t2.left)

  /** Backtracking over the order of each overlapping pair. After every guess,
    * transitivity and the parity constraints are propagated to a fixpoint; a
    * contradiction backtracks. */
  private final class Solver(n: Int, cons: Vector[Vector[(Int, Int)]],
      pairs: Vector[(Int, Int)], forced: Vector[(Int, Int)], budget: Int):
    private val rel = Array.ofDim[Byte](n, n)
    private val trail = mutable.ArrayBuffer.empty[(Int, Int)]
    private val queue = mutable.Queue.empty[(Int, Int)]
    private val touching = mutable.HashMap.empty[Long, mutable.ArrayBuffer[Int]]
    private var failed = false
    var steps = 0
    var gaveUp = false

    for (c, i) <- cons.zipWithIndex; (a, b) <- c do
      touching.getOrElseUpdate(key(a, b), mutable.ArrayBuffer.empty) += i

    private def key(i: Int, j: Int): Long =
      if i < j then i.toLong * n + j else j.toLong * n + i

    private def set(i: Int, j: Int, v: Byte): Unit =
      if failed then ()
      else if i == j then failed = true
      else if rel(i)(j) == v then ()
      else if rel(i)(j) != 0 then failed = true
      else
        rel(i)(j) = v
        rel(j)(i) = (-v).toByte
        trail += ((i, j))
        queue.enqueue((i, j))

    private def propagate(): Boolean =
      while queue.nonEmpty && !failed do
        val (p, q) = queue.dequeue()
        val (above, below) = if rel(p)(q) == 1 then (p, q) else (q, p)
        var k = 0
        while k < n && !failed do
          if rel(k)(above) == 1 then set(k, below, 1)
          if rel(below)(k) == 1 then set(above, k, 1)
          k += 1
        for i <- touching.getOrElse(key(p, q), mutable.ArrayBuffer.empty) if !failed do
          // An even number of the literals may hold; with one unknown left, it
          // is determined.
          val lits = cons(i)
          var trues = 0
          var unknown = -1
          for (a, b) <- lits do
            rel(a)(b) match
              case 1  => trues += 1
              case -1 => ()
              case _  => unknown = if unknown == -2 then -2 else if unknown >= 0 then -2 else lits.indexOf((a, b))
          if unknown == -1 then { if trues % 2 == 1 then failed = true }
          else if unknown >= 0 then
            val (a, b) = lits(unknown)
            set(a, b, if trues % 2 == 1 then 1 else -1)
      if failed then queue.clear()
      !failed

    private def undo(mark: Int): Unit =
      failed = false
      queue.clear()
      while trail.length > mark do
        val (i, j) = trail.remove(trail.length - 1)
        rel(i)(j) = 0
        rel(j)(i) = 0

    private def search(from: Int): Boolean =
      steps += 1
      if steps > budget then { gaveUp = true; return false }
      var k = from
      while k < pairs.length && rel(pairs(k)._1)(pairs(k)._2) != 0 do k += 1
      if k == pairs.length then true
      else
        val (i, j) = pairs(k)
        List[Byte](1, -1).exists: v =>
          val mark = trail.length
          set(i, j, v)
          if propagate() && search(k + 1) then true else { undo(mark); false }

    def run(): Option[Stacking] =
      for (above, below) <- forced do set(above, below, 1)
      if !propagate() || !search(0) then None
      else
        // Any linear extension of the order we ended up with.
        val higher = Array.fill(n)(mutable.ArrayBuffer.empty[Int])
        val indeg = Array.fill(n)(0)
        for i <- 0 until n; j <- 0 until n if rel(i)(j) == 1 do
          higher(j) += i
          indeg(i) += 1
        val ready = mutable.Queue.from((0 until n).filter(indeg(_) == 0))
        val order = mutable.ArrayBuffer.empty[Int]
        while ready.nonEmpty do
          val f = ready.dequeue()
          order += f
          for g <- higher(f) do
            indeg(g) -= 1
            if indeg(g) == 0 then ready.enqueue(g)
        Option.when(order.length == n)(Stacking(order.toVector))
