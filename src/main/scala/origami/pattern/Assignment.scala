package origami.pattern

enum Assignment:
  case Mountain, Valley, Boundary, Flat, Unassigned
  /** Undecided like `Unassigned`, but the search may also leave it flat. */
  case Optional

  def isFolded: Boolean = this == Mountain || this == Valley
  /** Folds in the model: mountain, valley, or unassigned, where the layer order decides which. */
  def folds: Boolean = isFolded || this == Unassigned
  /** Still to be decided by the search. */
  def isUndecided: Boolean = this == Unassigned || this == Optional
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
    case Optional   => "O"

object Assignment:
  val mv: List[Assignment] = List(Mountain, Valley)

  def alternating(i: Int, onEven: Assignment = Valley): Assignment =
    if i % 2 == 0 then onEven else onEven.opposite
