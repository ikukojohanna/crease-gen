package origami.pattern

import origami.geometry.Polygon

final case class Facet(index: Int, vertices: Vector[Int], polygon: Polygon)
