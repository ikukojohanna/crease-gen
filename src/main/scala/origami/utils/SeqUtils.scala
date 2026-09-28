package origami.utils

object SeqUtils:

  extension [A](xs: IndexedSeq[A])
    def cyclicPairs: Vector[(A, A)] =
      xs.indices.toVector.map(i => (xs(i), xs((i + 1) % xs.length)))

    def cyclicTriples: Vector[(A, A, A)] =
      val n = xs.length
      xs.indices.toVector.map(i => (xs((i + n - 1) % n), xs(i), xs((i + 1) % n)))

  extension [A](xs: IterableOnce[A])
    def distinctWith(same: (A, A) => Boolean): Vector[A] =
      xs.iterator.foldLeft(Vector.empty[A]): (acc, x) =>
        if acc.exists(same(_, x)) then acc else acc :+ x

    def distinctConsecutiveWith(same: (A, A) => Boolean): Vector[A] =
      xs.iterator.foldLeft(Vector.empty[A]): (acc, x) =>
        if acc.lastOption.exists(same(_, x)) then acc else acc :+ x
