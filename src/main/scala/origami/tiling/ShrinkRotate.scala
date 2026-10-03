package origami.tiling

import origami.geometry.{Pt, Tol}
import origami.pattern.{Assignment, CreasePattern}

import scala.math.{Pi, atan2, cos, sin}

/** Bateman's shrink-and-rotate: every tile shrinks by `s` and turns by `alpha` about its centre, every shared edge
  * opens into a parallelogram pleat, and every interior vertex into a twist polygon.
  */
object ShrinkRotate:

  enum Problem:
    case Scale(s: Double)
    case Twist(degrees: Double)
    case PleatsFlip(s: Double, cosAlpha: Double)
    case NotSpiderWeb(violations: Vector[Reciprocal.Violation])
    case PleatTooBlunt(pleatDegrees: Double, limitDegrees: Double)

    def explain: String = this match
      case Scale(s)          => f"shrink factor $s%.3f must lie strictly between 0 and 1"
      case Twist(d)          => f"twist angle $d%.1f deg must lie strictly between -90 and 90, and not be 0"
      case PleatsFlip(s, c)  => f"s = $s%.3f is not below cos(alpha) = $c%.3f: the pleats would have no width or turn inside out"
      case NotSpiderWeb(vs)  => s"not a spider web at ${vs.length} shared edge(s), e.g. ${vs.head.explain}"
      case PleatTooBlunt(p, l) =>
        f"the pleats' acute angle ($p%.1f deg) must be smaller than every corner angle and its supplement " +
          f"(smallest $l%.1f deg); raise alpha or bring s closer to cos(alpha)"

  /** The acute angle of every pleat parallelogram. It does not depend on the tile. */
  def pleatAngle(s: Double, alpha: Double): Double = atan2(cos(alpha) - s, sin(alpha))

  def apply(t: Tiling, s: Double, alpha: Double)(using Tol): Either[Problem, CreasePattern] =
    if alpha < 0 then apply(t.mirrored, s, -alpha).map(_.transform(Tiling.mirror))
    else validate(t, s, alpha).toLeft(build(t, s, alpha))

  /** The pattern without checking the parameters first: only for showing where rejected ones go wrong. */
  def unchecked(t: Tiling, s: Double, alpha: Double)(using Tol): CreasePattern =
    if alpha < 0 then unchecked(t.mirrored, s, -alpha).transform(Tiling.mirror) else build(t, s, alpha)

  private def validate(t: Tiling, s: Double, alpha: Double)(using Tol): Option[Problem] =
    lazy val web = Reciprocal.check(t)
    lazy val tightest = t.interiorCorners.map(c => math.min(c.angle, Pi - c.angle)).minOption
    lazy val gamma = pleatAngle(s, alpha)
    if !(s > 0 && s < 1) then Some(Problem.Scale(s))
    else if !(alpha > 0 && alpha < Pi / 2) then Some(Problem.Twist(math.toDegrees(alpha)))
    else if s >= cos(alpha) then Some(Problem.PleatsFlip(s, cos(alpha)))
    else if web.nonEmpty then Some(Problem.NotSpiderWeb(web))
    else tightest.filter(cornerTooTight(_, gamma)).map(m => Problem.PleatTooBlunt(math.toDegrees(gamma), math.toDegrees(m)))

  /** Whether a corner is too sharp, or too blunt, for pleats of acute angle `gamma`. */
  def cornerTooTight(angle: Double, gamma: Double): Boolean = math.min(angle, Pi - angle) <= gamma

  /** A pleat's width across its creases and length along them, in the tiling's units. */
  def pleatSize(t: Tiling, e: SharedEdge, s: Double, alpha: Double): (Double, Double) =
    val d = t.tiles(e.right).centre.distTo(t.tiles(e.left).centre)
    (d * (cos(alpha) - s), s * t.points(e.u).distTo(t.points(e.v)))

  /** Pleats narrower than `minWidth` across or shorter than it along. */
  def narrowPleats(t: Tiling, s: Double, alpha: Double, minWidth: Double): Vector[SharedEdge] =
    t.sharedEdges.filter: e =>
      val (width, length) = pleatSize(t, e, s, alpha)
      math.min(width, length) < minWidth

  /** A shared edge's pleat: the left tile's copy of `u` and `v`, then the right tile's copy of `v` and `u`. */
  def pleat(t: Tiling, e: SharedEdge, s: Double, alpha: Double): Vector[Pt] =
    def moved(tile: Int, p: Int): Pt =
      val c = t.tiles(tile).centre
      c + (t.points(p) - c).rotate(alpha) * s
    Vector(moved(e.left, e.u), moved(e.left, e.v), moved(e.right, e.v), moved(e.right, e.u))

  /** Each pleat's strip runs under one tile and one twist and over the other two; which way round is free locally
    * but not globally. Tucking every strip under the tile above it (left of a vertical pleat) makes all twists alike,
    * and the layer solver accepts it, where an arbitrary choice usually has no stacking.
    */
  private def build(t: Tiling, s: Double, alpha: Double)(using tol: Tol): CreasePattern =
    def mountainSide(e: SharedEdge): Assignment =
      val d = t.points(e.v) - t.points(e.u)
      val leftIsAbove = tol.gt(d.x, 0) || (tol.isZero(d.x) && d.y > 0)
      if leftIsAbove then Assignment.Mountain else Assignment.Valley
    // The left tile's crease and the twist edge at u get one label, the right tile's and the twist edge at v the other.
    val creases = t.sharedEdges.flatMap: e =>
      val Vector(leftU, leftV, rightV, rightU) = pleat(t, e, s, alpha): @unchecked
      val x = mountainSide(e)
      Vector((leftU, leftV, x), (rightU, rightV, x.opposite), (leftU, rightU, x), (leftV, rightV, x.opposite))
    CreasePattern.blank(t.paper).creaseAll(creases)
