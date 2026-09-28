package origami.folding

import origami.geometry.{Pt, Seg, Tri}

object Overlap:

  def triangles(t1: Tri, t2: Tri, eps: Double): Boolean =
    !separated(t1, t2, eps) && !separated(t2, t1, eps)

  private def separated(t1: Tri, t2: Tri, eps: Double): Boolean =
    t1.edges.exists: (p, q) =>
      val n = (q - p).perp
      val len = n.norm
      if len < 1e-12 then false
      else
        val axis = n / len
        val (a1, a2) = (t1.vertices.map(v => (v - Pt.origin).dot(axis)),
          t2.vertices.map(v => (v - Pt.origin).dot(axis)))
        a1.max <= a2.min + eps || a2.max <= a1.min + eps

  /** Whether more than `eps` of `s` runs through the inside of the triangle. */
  def segmentThrough(s: Seg, t: Tri, eps: Double): Boolean =
    rangeInside(s, t).exists: (lo, hi) =>
      (hi - lo) * s.length > eps && t.strictlyContains(s.at((lo + hi) / 2), eps)

  /** The parameter range of `s` inside a counter-clockwise triangle, clipped one edge at a time. */
  private def rangeInside(s: Seg, t: Tri): Option[(Double, Double)] =
    t.edges.foldLeft(Option((0.0, 1.0)))((range, edge) => range.flatMap(clipToInnerSide(s, edge, _)))

  private def clipToInnerSide(s: Seg, edge: (Pt, Pt), range: (Double, Double)): Option[(Double, Double)] =
    val (p, q) = edge
    val (lo, hi) = range
    val inward = (q - p).perp
    val startDepth = (s.a - p).dot(inward)
    val change = (s.b - p).dot(inward) - startDepth
    val clipped =
      if math.abs(change) < 1e-15 then Option.unless(startDepth < 0)(range)
      else
        val crossing = -startDepth / change
        Some(if change > 0 then (math.max(lo, crossing), hi) else (lo, math.min(hi, crossing)))
    clipped.filterNot((lo, hi) => lo > hi)
