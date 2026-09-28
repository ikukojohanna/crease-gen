package origami.pattern

final case class Edge(u: Int, v: Int, assignment: Assignment):
  def ends: (Int, Int) = (u, v)
  def other(i: Int): Int = if i == u then v else u
  def key: (Int, Int) = Edge.key(u, v)
  def withAssignment(a: Assignment): Edge = Edge(u, v, a)

object Edge:
  def key(u: Int, v: Int): (Int, Int) = if u < v then (u, v) else (v, u)
