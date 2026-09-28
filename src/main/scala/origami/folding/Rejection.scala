package origami.folding

import origami.laws.Violation

enum Rejection:
  case LocalLaws(violations: Vector[Violation])
  case Layers(verdict: LayerVerdict)
  case NoLabelling(searched: Int)

  def explain: String = this match
    case LocalLaws(vs)  => s"${vs.length} local violation(s): ${vs.head.explain}"
    case Layers(v)      => v.describe
    case NoLabelling(n) => s"searched $n labelling(s), none of them folds"
