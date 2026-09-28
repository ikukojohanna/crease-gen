package origami.folding

final case class Stacking(order: Vector[Int]):
  lazy val layerOf: Map[Int, Int] = order.zipWithIndex.toMap
  def depth: Int = order.length
