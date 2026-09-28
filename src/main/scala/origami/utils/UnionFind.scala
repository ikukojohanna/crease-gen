package origami.utils

final class UnionFind(size: Int):
  private val parent = Array.tabulate(size)(identity)

  def find(i: Int): Int =
    if parent(i) != i then parent(i) = find(parent(i))
    parent(i)

  def union(a: Int, b: Int): Unit =
    val (ra, rb) = (find(a), find(b))
    if ra != rb then parent(ra) = rb
