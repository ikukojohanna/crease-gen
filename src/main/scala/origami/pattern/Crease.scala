package origami.pattern

import origami.geometry.{Pt, Seg}

final case class Crease(seg: Seg, assignment: Assignment):
  def a: Pt = seg.a
  def b: Pt = seg.b
  def withAssignment(x: Assignment): Crease = Crease(seg, x)
