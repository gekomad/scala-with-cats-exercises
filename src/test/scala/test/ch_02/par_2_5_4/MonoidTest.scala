package test.ch_02.par_2_5_4

import org.scalatest.FunSuite
import scala.collection.immutable

class MonoidTest extends FunSuite {

  test("sum two option") {
    val a: Option[Int] = Option(1)
    val b              = None
    val c: Option[Int] = a.map(_ + b.getOrElse(0))
    assert(c == Option(1))
  }

  test("sum two None") {
    val a: Option[Int] = None
    val b: Option[Int] = None

    val c: Option[Int] = a.map(_ + b.getOrElse(0))
    assert(c == None)
  }

  test("sum Option list") {
    val a: List[Option[Int]] = List(Option(1), None)

    val c = a.foldLeft(Option(0)) { (a, b) =>
      val p  = a.getOrElse(0)
      val p2 = b.getOrElse(0)
      Some(p + p2)
    }
    assert(c == Option(1))
  }

  test("sum None list") {
    val a: List[Option[Int]] = List(None, None)

    val c = a.foldLeft(Option(0)) { (a, b) =>
      val p  = a.getOrElse(0)
      val p2 = b.getOrElse(0)
      Some(p + p2)
    }
    assert(c == Option(0))
  }

  test("sum 2 option monoid") {

    import cats.instances.int._
    import cats.instances.option._
    import cats.syntax.semigroup._ // for |+|

    val a = Option(1)
    val b = None
    val c = a |+| b
    assert(c == Option(1))
  }

  test("sum 2 None monoid") {

    import cats.instances.int._
    import cats.instances.option._
    import cats.syntax.semigroup._ // for |+|

    val a: Option[Int] = None
    val b              = None
    val c              = a |+| b
    assert(c == None)
  }

  test("sum option monoid list") {
    import cats.Monoid
    import cats.instances.int._
    import cats.instances.option._
    import cats.syntax.semigroup._ // for |+|

    def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty)(_ |+| _)

    // alternatives
    def add2[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)

    val a: List[Option[Int]] = List(Option(1), None)

    val c  = add(a)
    val c2 = add2(a)

    assert(c == Option(1))
    assert(c2 == Option(1))
  }

  test("sum order monoid list") {
    import cats.Monoid
    import cats.syntax.semigroup._ // for |+|

    final case class Order(totalCost: Double, quantity: Double)

    implicit val monoid: Monoid[Order] = new Monoid[Order] {
      def combine(o1: Order, o2: Order) =
        Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)

      def empty = Order(0, 0)
    }

    def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty)(_ |+| _)

    // alternatives
    def add2[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)

    //    val a: List[Option[Int]] = List(Option(1), None)
    val a: List[Order] = List(Order(1, 2), Order(4, 5))

    val c  = add(a)
    val c2 = add2(a)

    assert(c == Order(5, 7))
    assert(c2 == Order(5, 7))
  }

  test("sum option order monoid list") {
    import cats.Monoid
    import cats.instances.option._
    import cats.syntax.semigroup._ // for |+|

    final case class Order(totalCost: Double, quantity: Double)

    implicit val monoid: Monoid[Order] = new Monoid[Order] {
      def combine(o1: Order, o2: Order) =
        Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)

      def empty = Order(0, 0)
    }

    def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(monoid.empty)(_ |+| _)

    // alternatives
    def add2[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)

    val a: List[Option[Order]] = List(Some(Order(1, 2)), Some(Order(4, 5)), None)

    val c  = add(a)
    val c2 = add2(a)

    assert(c == Some(Order(5, 7)))
    assert(c2 == Some(Order(5, 7)))
  }

  test("Monad test") {

    import cats.Monad
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    def ff[M[_]: Monad](a: M[Int], b: M[Int], f: (Int, Int) => Int): M[Int] =
      for {
        x <- a
        y <- b
      } yield f(x, y)

    import cats.instances.option._
    import cats.instances.list._
    import cats.Id
    val a1: Option[Int] = ff(Option(3), Option(1), _ + _)
    assert(a1 == Some(4))

    val a11: Id[Int] = ff(3: Id[Int], 1: Id[Int], _ + _)
    assert(a11 == 4)

    val a2: immutable.Seq[Int] = ff(List(1, 2, 3), List(10, 20), _ + _)
    assert(a2 == List(11, 21, 12, 22, 13, 23))
  }
}
