package origami.output

final case class FoldedOptions(
    size: Double = 340,
    margin: Double = 28,
    background: String = "#fffdf7",
    title: Option[String] = None,
    front: String = "#fdf3e3",
    back: String = "#e8d9c0",
    facetEdge: String = "#00000022",
    paperEdge: String = "#2c3e50",
    crease: String = "#0000001a"
)
