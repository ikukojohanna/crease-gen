package origami.laws

import origami.geometry.{Line, Pt}

object Symmetry:
  def mirror(l: Line): Pt => Pt = l.reflect
  def rotation(centre: Pt, turns: Int): Pt => Pt = p =>
    centre + (p - centre).rotate(2 * math.Pi / turns)
  def halfTurn(centre: Pt): Pt => Pt = rotation(centre, 2)
