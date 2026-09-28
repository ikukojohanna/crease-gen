package origami.laws

import origami.geometry.Tol
import origami.pattern.{CreasePattern, PlanarGraph}

/** A graph that has passed `Laws.checkAll`; there is no other way to get one. */
opaque type FlatFoldable = PlanarGraph

object FlatFoldable:
  def from(g: PlanarGraph)(using Tol): Either[Vector[Violation], FlatFoldable] =
    Laws.checkAll(g) match
      case v if v.isEmpty => Right(g)
      case v              => Left(v)

  def from(cp: CreasePattern)(using Tol): Either[Vector[Violation], FlatFoldable] =
    from(cp.planarize)

  extension (f: FlatFoldable)
    def graph: PlanarGraph = f
    def pattern(using Tol): CreasePattern = f.toPattern
