package origami.folding

import scala.collection.mutable

/** Finds a stacking by backtracking over the order of each overlapping pair.
  *
  * After every guess the consequences are propagated: transitivity, and the parity constraints of
  * `StackingProblem`. A contradiction undoes the guess.
  */
private[folding] final class LayerSolver(facetCount: Int, constraints: Vector[Vector[(Int, Int)]],
    pairs: Vector[(Int, Int)], forced: Vector[(Int, Int)], budget: Int):
  import LayerSolver.{Above, Below, Open}

  private val relation = Array.ofDim[Byte](facetCount, facetCount)
  private val decisions = mutable.ArrayBuffer.empty[(Int, Int)]
  private val unpropagated = mutable.Queue.empty[(Int, Int)]
  private val constraintsOn: Map[Long, Vector[Int]] =
    constraints.zipWithIndex
      .flatMap((relations, c) => relations.map((a, b) => (pairKey(a, b), c)))
      .groupMap(_._1)(_._2)
  private var contradiction = false

  var steps = 0
  var gaveUp = false

  def run(): Option[Stacking] =
    forced.foreach((top, bottom) => decide(top, bottom, Above))
    if propagate() && search(0) then bottomToTop else None

  private def isAbove(a: Int, b: Int): Boolean = relation(a)(b) == Above
  private def isOpen(a: Int, b: Int): Boolean = relation(a)(b) == Open

  private def pairKey(a: Int, b: Int): Long =
    if a < b then a.toLong * facetCount + b else b.toLong * facetCount + a

  private def search(from: Int): Boolean =
    steps += 1
    if steps > budget then
      gaveUp = true
      false
    else
      pairs.indexWhere(isOpen, from) match
        case -1 => true
        case k =>
          val (a, b) = pairs(k)
          List(Above, Below).exists: guess =>
            val mark = decisions.length
            decide(a, b, guess)
            propagate() && search(k + 1) || { undoTo(mark); false }

  private def decide(a: Int, b: Int, value: Byte): Unit =
    if contradiction || relation(a)(b) == value then ()
    else if a == b || !isOpen(a, b) then contradiction = true
    else
      relation(a)(b) = value
      relation(b)(a) = (-value).toByte
      decisions += ((a, b))
      unpropagated.enqueue((a, b))

  private def undoTo(mark: Int): Unit =
    contradiction = false
    unpropagated.clear()
    while decisions.length > mark do
      val (a, b) = decisions.remove(decisions.length - 1)
      relation(a)(b) = Open
      relation(b)(a) = Open

  private def propagate(): Boolean =
    while unpropagated.nonEmpty && !contradiction do
      val (a, b) = unpropagated.dequeue()
      val (top, bottom) = if isAbove(a, b) then (a, b) else (b, a)
      enforceTransitivity(top, bottom)
      for c <- constraintsOn.getOrElse(pairKey(a, b), Vector.empty) if !contradiction do
        enforceParity(constraints(c))
    if contradiction then unpropagated.clear()
    !contradiction

  /** Whatever is above `top` is above `bottom`, and whatever is below `bottom` is below `top`. */
  private def enforceTransitivity(top: Int, bottom: Int): Unit =
    for k <- 0 until facetCount if !contradiction do
      if isAbove(k, top) then decide(k, bottom, Above)
      if isAbove(bottom, k) then decide(top, k, Above)

  /** An even number of the relations hold, so once all but one are decided, the last one is too. */
  private def enforceParity(relations: Vector[(Int, Int)]): Unit =
    val oddHolding = relations.count(isAbove) % 2 == 1
    relations.count(isOpen) match
      case 0 => if oddHolding then contradiction = true
      case 1 => relations.find(isOpen).foreach((a, b) => decide(a, b, if oddHolding then Above else Below))
      case _ => ()

  /** Any order of the facets consistent with the decided relations, bottom first. */
  private def bottomToTop: Option[Stacking] =
    val facetsAbove = Array.fill(facetCount)(mutable.ArrayBuffer.empty[Int])
    val unplacedBelow = Array.fill(facetCount)(0)
    for a <- 0 until facetCount; b <- 0 until facetCount if isAbove(a, b) do
      facetsAbove(b) += a
      unplacedBelow(a) += 1

    val ready = mutable.Queue.from((0 until facetCount).filter(unplacedBelow(_) == 0))
    val order = mutable.ArrayBuffer.empty[Int]
    while ready.nonEmpty do
      val f = ready.dequeue()
      order += f
      for g <- facetsAbove(f) do
        unplacedBelow(g) -= 1
        if unplacedBelow(g) == 0 then ready.enqueue(g)
    Option.when(order.length == facetCount)(Stacking(order.toVector))

private[folding] object LayerSolver:
  private val Above: Byte = 1
  private val Below: Byte = -1
  private val Open: Byte = 0
