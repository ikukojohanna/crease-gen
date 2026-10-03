package origami.output

import origami.geometry.Tol
import origami.pattern.{Assignment, CreasePattern, Faces, PlanarGraph}

object Fold:

  def apply(g: PlanarGraph, name: String)(using Tol): String =
    val verts = g.vertices.map(p => f"[${p.x}%.6f, ${p.y}%.6f]").mkString(", ")
    val edges = g.edges.map(e => s"[${e.u}, ${e.v}]").mkString(", ")
    val assign = g.edges.map(e => "\"" + foldCode(e.assignment) + "\"").mkString(", ")
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

  /** The FOLD spec has no optional crease: to anything reading the file it is just unassigned. */
  private def foldCode(a: Assignment): String =
    if a == Assignment.Optional then Assignment.Unassigned.code else a.code

  private def quote(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
