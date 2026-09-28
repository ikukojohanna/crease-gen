package origami.pattern

enum Assignment:
  case Mountain, Valley, Boundary, Flat, Unassigned

  def isFolded: Boolean = this == Mountain || this == Valley
  def opposite: Assignment = this match
    case Mountain => Valley
    case Valley   => Mountain
    case other    => other

  /** In degrees, FOLD format convention. */
  def foldAngle: Double = this match
    case Mountain => -180
    case Valley   => 180
    case _        => 0

  def code: String = this match
    case Mountain => "M"
    case Valley   => "V"
    case Boundary => "B"
    case Flat     => "F"
    case Unassigned => "U"

object Assignment:
  val mv: List[Assignment] = List(Mountain, Valley)

  def alternating(i: Int, onEven: Assignment = Valley): Assignment =
    if i % 2 == 0 then onEven else onEven.opposite
