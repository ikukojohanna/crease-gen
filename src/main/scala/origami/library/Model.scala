package origami.library

import origami.geometry.{Pt, Tol}
import origami.laws.{FlatFoldable, Violation}
import origami.pattern.CreasePattern

final case class Model(name: String, pattern: CreasePattern, note: String = "",
    caveat: Option[String] = None,
    symmetries: Vector[Pt => Pt] = Vector.empty):
  def check(using Tol): Either[Vector[Violation], FlatFoldable] = FlatFoldable.from(pattern)
