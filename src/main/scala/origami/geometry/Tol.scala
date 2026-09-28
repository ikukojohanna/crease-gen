package origami.geometry

import scala.math.abs

/** Tolerance used by every geometric comparison. */
final case class Tol(value: Double):
  def isZero(a: Double): Boolean = abs(a) <= value
  def eqv(a: Double, b: Double): Boolean = abs(a - b) <= value
  def lt(a: Double, b: Double): Boolean = a < b - value
  def gt(a: Double, b: Double): Boolean = a > b + value

object Tol:
  given default: Tol = Tol(1e-7)
