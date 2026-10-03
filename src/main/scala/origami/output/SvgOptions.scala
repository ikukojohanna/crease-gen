package origami.output

import origami.geometry.Pt
import origami.pattern.Assignment

final case class SvgOptions(
    scale: Double = 1.0,
    margin: Double = 24,
    background: String = "#fffdf7",
    title: Option[String] = None,
    showVertices: Boolean = false,
    showLegend: Boolean = true,
    mountain: Style = Style("#c0392b", 1.6, None),
    valley: Style = Style("#2471a3", 1.6, Some("7 4")),
    boundary: Style = Style("#2c3e50", 2.2, None),
    flat: Style = Style("#b8b8b0", 1.0, Some("2 4")),
    unassigned: Style = Style("#7f8c8d", 1.4, Some("1 3")),
    /** Points to ring, such as where a law fails. */
    marks: Vector[Pt] = Vector.empty
):
  def styleFor(a: Assignment): Style = a match
    case Assignment.Mountain   => mountain
    case Assignment.Valley     => valley
    case Assignment.Boundary   => boundary
    case Assignment.Flat       => flat
    case Assignment.Unassigned => unassigned
    case Assignment.Optional   => unassigned
