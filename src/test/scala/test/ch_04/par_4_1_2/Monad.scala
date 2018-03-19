package test.ch_04.par_4_1_2

import org.scalatest.FunSuite

class Monad extends FunSuite {

  test("monad1") {
    import scala.language.higherKinds

    trait Monad[F[_]] {
      def pure[A](a: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

      def map[A, B](value: F[A])(func: A => B): F[B] =
        flatMap(value)(a => pure(func(a)))
    }

  }

  test("monad2") {
    import cats.Monad
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad

    val opt1 = Monad[Option].pure(3)
    assert(opt1 == Option(3))

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    assert(opt2 == Option(5))

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    assert(opt3 == Option(500))

    val list1 = Monad[List].pure(3)
    assert(list1 == List(3))

    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    assert(list2 == List(1, 10, 2, 20, 3, 30))

    val list3 = Monad[List].map(list2)(a => a + 123)
    assert(list3 == List(124, 133, 125, 143, 126, 153))
  }

  test("monad3") {
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad
    import cats.syntax.applicative._ // for pure
    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.Monad
    import scala.concurrent.{Await, Future}
    import cats.instances.future._ // for Monad
    import scala.concurrent.duration._

    val a1 = 1.pure[Option]
    assert(a1 == Some(1))

    val a2 = 1.pure[List]
    assert(a2 == List(1))

    {
      val o: Future[Int] = 1.pure[Future]
      val p = Await.result(o, Duration.Inf)
      assert(p == 1)
    }

    {
      val fm = Monad[Future]

      val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
      val p = Await.result(future, 1.second)
      assert(p == 3)
    }


  }
}