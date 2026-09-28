package origami.geometry

import scala.math.{atan2, hypot}

final case class Vec(x: Double, y: Double):
  def +(o: Vec): Vec = Vec(x + o.x, y + o.y)
  def -(o: Vec): Vec = Vec(x - o.x, y - o.y)
  def *(s: Double): Vec = Vec(x * s, y * s)
  def /(s: Double): Vec = Vec(x / s, y / s)
  def unary_- : Vec = Vec(-x, -y)
  def dot(o: Vec): Double = x * o.x + y * o.y
  def cross(o: Vec): Double = x * o.y - y * o.x
  def normSq: Double = x * x + y * y
  def norm: Double = hypot(x, y)
  def normalized: Vec = this / norm
  /** A quarter turn counter-clockwise. */
  def perp: Vec = Vec(-y, x)
  def rotate(theta: Double): Vec =
    val (c, s) = (math.cos(theta), math.sin(theta))
    Vec(x * c - y * s, x * s + y * c)
  def angle: Double = Geometry.norm2Pi(atan2(y, x))
  def isZero(using tol: Tol): Boolean = tol.isZero(norm)

object Vec:
  val zero: Vec = Vec(0, 0)
  def polar(r: Double, theta: Double): Vec = Vec(r * math.cos(theta), r * math.sin(theta))
