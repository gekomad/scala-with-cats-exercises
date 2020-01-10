package test.ch_06.par_6_3_1_1

import org.scalatest.FunSuite

class SemigroupTest extends FunSuite {

  test("semigroup") {
    import cats.Semigroupal
    final case class Cat(name: String, born: Option[Int], color: String)
    import cats.instances.option._
    import cats.syntax.apply._ // for tupled and mapN

    {
      val l = (Option(123), Option("abc")).tupled
      assert(l == Some((123, "abc")))
    }

    {
      val l: Option[Cat] = (Option("Garfield"), Option(Some(1978)), Option("Orange & black")).mapN(Cat.apply)

      assert(l == Some(Cat("Garfield", Some(1978), "Orange & black")))
    }

    {
      val l: Option[Cat] = (Option("Garfield"), None, Option("Orange & black")).mapN(Cat.apply)

      assert(l == None)
    }

  }

  test("|+|") {
    import cats.Monoid
    import cats.instances.int._
    import cats.instances.invariant._
    import cats.instances.list._
    import cats.instances.string._
    import cats.syntax.apply._ // for  catsSemigroupalForMonoid: InvariantSemigroupal[Monoid]

    final case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

    val apply1: (String, Int, List[String]) => Cat = Cat.apply _

    val unapply1: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(apply1)(unapply1)

    import cats.syntax.semigroup._ // for |+|

    val garfield = Cat("Garfield", 1978, List("Lasagne"))

    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

    val l = garfield |+| heathcliff

    assert(l == Cat("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food")))
  }

  test("future") {

    import cats.Semigroupal
    import cats.instances.future._
    import cats.instances.option._
    import cats.syntax.apply._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    {
      val futurePair: Future[(String, Int)] = Semigroupal[Future].product(Future("Hello"), Future(123))
      val d                                 = Await.result(futurePair, 1.second)
      assert(d == ("Hello", 123))
    }

    {
      val optionPair: Option[(String, Int)] = Semigroupal[Option].product(Option("Hello"), Option(123))
      assert(optionPair == Some("Hello", 123))
    }

    {
      final case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])
      val futureCat = (Future("Garfield"), Future(1978), Future(List("Lasagne"))).mapN(Cat.apply)
      val o: Cat    = Await.result(futureCat, Duration.Inf)
      assert(o == Cat("Garfield", 1978, List("Lasagne")))
    }
  }

  test("The Product of Monads") {
    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      for {
        a <- x
        b <- y
      } yield (a, b)

    import cats.instances.list._ // for Semigroupal

    val a = product(List(1, 2), List(3, 4))
    assert(a == List((1, 3), (1, 4), (2, 3), (2, 4)))
    import cats.instances.all._

    type ErrorOr[A] = Either[Vector[String], A]

    val b = product[ErrorOr, Int, Int](Left(Vector("Error 1")), Left(Vector("Error 2")))
    assert(b == Left(Vector("Error 1")))

  }

}
