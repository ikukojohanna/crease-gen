package origami.geometry

import scala.math.Pi

object Geometry:
  def norm2Pi(a: Double): Double =
    val r = a % (2 * Pi)
    if r < 0 then r + 2 * Pi else r

  def degrees(d: Double): Double = d * Pi / 180
