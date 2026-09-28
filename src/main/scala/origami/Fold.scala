package origami

/** Export to the FOLD format (github.com/edemaine/fold), the interchange format
  * for computational origami.
  *
  * Worth having because it closes the loop: the pattern this program computes
  * can be dropped into Origami Simulator and folded on screen, then printed and
  * folded in paper. Same data, three materials.
  *
  * A FOLD file needs the faces as well as the creases, which is the same
  * observation the rest of this program keeps running into: a set of lines is
  * not yet a sheet of paper. `Faces` supplies them, counter-clockwise, as the
  * format requires.
  */
object Fold:

  def apply(g: PlanarGraph, name: String)(using Tol): String =
    val verts = g.vertices.map(p => f"[${p.x}%.6f, ${p.y}%.6f]").mkString(", ")
    val edges = g.edges.map(e => s"[${e.u}, ${e.v}]").mkString(", ")
    val assign = g.edges.map(e => "\"" + e.assignment.code + "\"").mkString(", ")
    val angles = g.edges.map(e => f"${e.assignment.foldAngle}%.1f").mkString(", ")
    val faces = Faces(g).facets.map(_.vertices.mkString("[", ", ", "]")).mkString(", ")
    s"""{
  "file_spec": 1.1,
  "file_creator": "folding (Scala crease pattern generator)",
  "file_classes": ["singleModel"],
  "frame_title": ${quote(name)},
  "frame_classes": ["creasePattern"],
  "frame_attributes": ["2D"],
  "vertices_coords": [$verts],
  "edges_vertices": [$edges],
  "edges_assignment": [$assign],
  "edges_foldAngle": [$angles],
  "faces_vertices": [$faces]
}
"""

  def apply(cp: CreasePattern, name: String)(using Tol): String = apply(cp.planarize, name)

  private def quote(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
