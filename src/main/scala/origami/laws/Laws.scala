package origami.laws

import origami.geometry.Tol
import origami.pattern.{Assignment, PlanarGraph, VertexInfo}

import scala.math.abs

object Laws:

  def kawasaki(v: VertexInfo)(using tol: Tol): Option[Violation] =
    if v.degree == 0 then None
    else if v.degree % 2 != 0 then Some(Violation.OddDegree(v.at, v.degree))
    else
      val s = v.sectors.zipWithIndex.map((a, i) => if i % 2 == 0 then a else -a).sum
      Option.unless(tol.isZero(s))(Violation.Kawasaki(v.at, s))

  def maekawa(g: PlanarGraph, v: VertexInfo): Option[Violation] =
    if v.degree == 0 then None
    else
      val kinds = v.foldedEdges.map(g.assignment)
      val m = kinds.count(_ == Assignment.Mountain)
      val n = kinds.count(_ == Assignment.Valley)
      Option.unless(abs(m - n) == 2)(Violation.Maekawa(v.at, m, n))

  def bigLittleBig(g: PlanarGraph, v: VertexInfo)(using Tol): Vector[Violation] =
    v.littleSectors.flatMap: s =>
      val (left, right) = (g.assignment(s.left), g.assignment(s.right))
      Option.when(left == right && left.isFolded)(Violation.BigLittleBig(v.at, s.angle))

  def undecided(g: PlanarGraph, v: VertexInfo): Option[Violation] =
    Option.when(v.foldedEdges.exists(e => g.assignment(e).isUndecided))(
      Violation.Undecided(v.at))

  def deadEnd(v: VertexInfo): Option[Violation] =
    Option.when(v.degree == 1)(Violation.DeadEnd(v.at, v.degree))

  def checkGeometry(g: PlanarGraph)(using Tol): Vector[Violation] =
    g.interiorVertices.flatMap(v => deadEnd(v).toVector ++ kawasaki(v).toVector)

  def checkAll(g: PlanarGraph)(using Tol): Vector[Violation] =
    g.interiorVertices.flatMap: v =>
      undecided(g, v).toVector ++ deadEnd(v).toVector ++ kawasaki(v).toVector ++
        maekawa(g, v).toVector ++ bigLittleBig(g, v)
