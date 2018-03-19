package test.ch_06.par_7_2_2_1


import org.scalatest.FunSuite

class TraverseTest extends FunSuite {

  import cats.instances.vector._
  import scala.language.higherKinds
  import cats.syntax.apply._ // for mapN
  import cats.Applicative
  import cats.syntax.applicative._ // for pure

  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] = listTraverse(list)(identity)

  test("7.2.2.1 Traversing with Vectors") {

    val o = listSequence(List(Vector(1, 2), Vector(3, 4)))
    assert(o == Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
  }

  test("7.2.2.2 Traversing with Options") {
    import cats.instances.option._ // for Applicative
    def process(inputs: List[Int]) = listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

    val a: Option[List[Int]] = process(List(2, 4, 6))
    val b: Option[List[Int]] = process(List(1, 2, 3))

    assert(a == Some(List(2, 4, 6)))
    assert(b == None)

  }

  test("7.2.2.3 Traversing with Validated") {
    import cats.data.Validated
    import cats.data.Validated.{Invalid, Valid}

    import cats.instances.list._ // for Monoid
    type ErrorsOr[A] = Validated[List[String], A]

    def process(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if (n % 2 == 0) Validated.valid(n) else
          Validated.invalid(List(s"$n is not even"))
      }

    val a = process(List(2, 4, 6))
    val b = process(List(1, 2, 3))
    assert(a == Valid(List(2, 4, 6)))
    assert(b == Invalid(List("1 is not even", "3 is not even")))

  }
}