package origami.laws

import origami.geometry.Pt

import scala.math.Pi

enum Violation(val where: Pt):
  case DeadEnd(at: Pt, degree: Int) extends Violation(at)
  case OddDegree(at: Pt, degree: Int) extends Violation(at)
  case Kawasaki(at: Pt, alternatingSum: Double) extends Violation(at)
  case Maekawa(at: Pt, mountains: Int, valleys: Int) extends Violation(at)
  case BigLittleBig(at: Pt, sector: Double) extends Violation(at)
  case Undecided(at: Pt) extends Violation(at)
  case Inconsistent(at: Pt) extends Violation(at)

  def explain: String =
    val here = f"(${where.x}%.2f, ${where.y}%.2f)"
    this match
      case DeadEnd(_, d)      => s"$here: a crease ends in mid-sheet (degree $d)"
      case OddDegree(_, d)    => s"$here: $d creases meet, but Maekawa needs an even number"
      case Kawasaki(_, s)     => f"$here: Kawasaki fails, alternating sum ${s * 180 / Pi}%.3f deg (must be 0)"
      case Maekawa(_, m, v)   => s"$here: Maekawa fails, $m mountains and $v valleys (|M-V| must be 2)"
      case BigLittleBig(_, a) => f"$here: the smallest sector (${a * 180 / Pi}%.1f deg) is bounded by two creases of the same kind"
      case Undecided(_)       => s"$here: an unassigned crease meets this vertex"
      case Inconsistent(_)    => s"$here: the folding map disagrees with itself across this crease"
