package test.ch_03.par_3_5


import org.scalatest.FunSuite

import scala.collection.immutable

class FunctorTest extends FunSuite {

  test("examples") {
    import scala.language.higherKinds // or add to build.sbt: scalacOptions += "-language:higherKinds"
    import cats.Functor
    import cats.instances.list._

    import cats.instances.option._ // for Functor

    val list1 = List(1, 2, 3)

    val list2: immutable.Seq[Int] = Functor[List].map(list1)(_ * 2)
    assert(list2 == List(2, 4, 6))

    val option1 = Option(123)
    assert(option1 == Some(123))

    val s = option1.map(_.toString)
    assert(s == Some("123"))

    val option2 = Functor[Option].map(option1)(_.toString)
    assert(option2 == Some("123"))


    import cats.instances.option._

    //    def func(x: Int) = x + 1
    val func = (x: Int) => x + 1 //function1

    val liftedFunc: Option[Int] => Option[Int] = Functor[Option].lift(func)

    val o: Option[Int] = liftedFunc(Option(1))
    val o2: Option[Int] = liftedFunc(None)

    assert(o == Some(2))
    assert(o2 == None)
  }

  test("functor syntax") {
    import cats.instances.function._ // for Functor
    import cats.syntax.functor._ // for map

    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => a + "!"
    val func4 = func1.map(func2).map(func3)
    val r: String = func4(123)
    assert(r == "248!")
  }

  test("abstract functor") {
    import cats.syntax.functor._ // for map
    import cats.instances.option._ // for Functor
    import cats.instances.list._ // for Functor
    import cats.Functor
    import scala.language.higherKinds // or add to build.sbt: scalacOptions += "-language:higherKinds"

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)

    val a: Option[Int] = doMath(Option(20))
    assert(a == Some(22))

    val b: immutable.Seq[Int] = doMath(List(1, 2, 3))
    assert(b == List(3, 4, 5))
  }

  test("Monad[Option] Monad[List]") {

    import cats.Monad
    import cats.instances.option._
    import cats.instances.list._


    val opt1: Option[Int] = Monad[Option].pure(3)

    val opt0 = opt1.flatMap(a => Some(a + 2))
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)

    val list1 = Monad[List].pure(3)

    val list2 = Monad[List].flatMap(List(1, 2, 3))(x => List(x, x * 10))
    val list22 = Monad[List].flatMap(List(1, 2, 3))(x => List(x + 1))


    val list2u2 = List(1, 2, 3).flatMap(x => List(x + 1))

    val list3 = Monad[List].map(list2)(_ + 123)

    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.future._
    import scala.concurrent._
    import scala.concurrent.duration._

    val fm = Monad[Future]

    val pp = Await.result(
      Monad[Future].flatMap(fm.pure(1)) { x =>
        fm.pure(x + 2)
      },
      1.second
    )
    assert(pp == 3)
  }
}