package origami.folding

enum LayerVerdict:
  case Stacked(stacking: Stacking, overlaps: Int, constraints: Int)
  case Impossible(overlaps: Int, constraints: Int)
  case GaveUp(steps: Int, overlaps: Int, constraints: Int)

  def ok: Boolean = this.isInstanceOf[LayerVerdict.Stacked]

  def describe: String = this match
    case Stacked(s, o, c) => s"stacks in ${s.depth} layers ($o overlapping pairs, $c constraints)"
    case Impossible(o, c) => s"no stacking exists ($o overlapping pairs, $c constraints)"
    case GaveUp(n, o, c)  => s"gave up after $n steps ($o overlapping pairs, $c constraints)"
