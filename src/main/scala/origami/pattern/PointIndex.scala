package origami.pattern

import origami.geometry.{Pt, Tol}

import scala.collection.mutable

/** Numbers points, giving points within tolerance of each other the same number. */
private[origami] final class PointIndex(tol: Tol):
  private val cellSize = tol.value * 4
  private val cells = mutable.HashMap.empty[(Long, Long), mutable.ArrayBuffer[Int]]
  private val points = mutable.ArrayBuffer.empty[Pt]

  def all: Vector[Pt] = points.toVector

  def indexOf(p: Pt): Int =
    nearby(p).find(i => points(i).distTo(p) <= tol.value).getOrElse(insert(p))

  private def cellOf(p: Pt): (Long, Long) =
    (math.floor(p.x / cellSize).toLong, math.floor(p.y / cellSize).toLong)

  private def nearby(p: Pt): Iterator[Int] =
    val (cx, cy) = cellOf(p)
    for
      dx <- Iterator(-1, 0, 1)
      dy <- Iterator(-1, 0, 1)
      i <- cells.get((cx + dx, cy + dy)).iterator.flatten
    yield i

  private def insert(p: Pt): Int =
    val i = points.length
    points += p
    cells.getOrElseUpdate(cellOf(p), mutable.ArrayBuffer.empty) += i
    i
