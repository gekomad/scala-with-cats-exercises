package test.ch_11

import org.scalatest.FunSuite

class BoundedSemiLattice extends FunSuite {

  test("11.2.3") {
    final case class GCounter(counters: Map[String, Int]) {

      def increment(machine: String, amount: Int): GCounter = {
        val value = amount + counters.getOrElse(machine, 0)
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter): GCounter =
        GCounter(that.counters ++ this.counters.map {
          case (k, v) =>
            k -> (v max that.counters.getOrElse(k, 0))
        })

      def total: Int = counters.values.sum
    }

    val g1 = GCounter(Map("A" -> 1, "B" -> 2))

    assert(g1.increment("A", 4) == GCounter(Map("A" -> 5, "B" -> 2)))

    val g2 = GCounter(Map("A" -> 1, "B" -> 3))

    assert(g1.merge(g2) == GCounter(Map("A" -> 1, "B" -> 3)))

    assert(g1.total == 3)
  }

  import cats.Monoid

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A

    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intInstance: BoundedSemiLattice[Int] =
      new BoundedSemiLattice[Int] {

        def combine(a1: Int, a2: Int): Int = a1 max a2

        val empty: Int = 0
      }

    implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
      new BoundedSemiLattice[Set[A]] {

        def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

        val empty: Set[A] = Set.empty[A]
      }
  }

  test("11.3.2") {
    assert(BoundedSemiLattice.intInstance.combine(2, 3) == 3)
    assert(BoundedSemiLattice.setInstance.combine(Set(1, 2), Set(3)) == Set(1, 2, 3))
    assert(BoundedSemiLattice.setInstance.combine(Set(1, 2), Set(2)) == Set(1, 2))
  }

  ignore("11.3.3") {

    import BoundedSemiLattice._
    import cats.Monoid
    import cats.instances.list._
    import cats.instances.map._
    import cats.syntax.foldable._
    import cats.syntax.semigroup._

    final case class GCounter[A](counters: Map[String, A]) {

      def increment(machine: String, amount: A)(implicit m: Monoid[A]) = {
        val value = amount |+| counters.getOrElse(machine, m.empty)
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
        GCounter(this.counters |+| that.counters)

      def total(implicit m: Monoid[A]): A =
        this.counters.values.toList.combineAll
    }

    val g1 = GCounter(Map("A" -> 4, "B" -> 2))

    assert(g1.increment("A", 1) == GCounter(Map("A" -> 5, "B" -> 2)))

    assert(g1.merge(GCounter(Map("A" -> 1, "B" -> 3))) == GCounter(Map("A" -> 4, "B" -> 3)))

    assert(g1.total == 6)

  }
}