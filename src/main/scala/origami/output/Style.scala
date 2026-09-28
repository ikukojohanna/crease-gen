package origami.output

final case class Style(stroke: String, width: Double, dash: Option[String]):
  def attrs: String =
    s"""stroke="$stroke" stroke-width="$width"""" + dash.fold("")(d => s""" stroke-dasharray="$d"""")
