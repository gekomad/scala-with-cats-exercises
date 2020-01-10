package test.ch_04.par_4_7_3

import cats.Id
import cats.data.WriterT
import org.scalatest.FunSuite

class WriterMonadTest extends FunSuite {

  test("factorial writer") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, Future}

    import cats.data.Writer
    import cats.instances.vector._
    import cats.syntax.applicative._
    import cats.syntax.writer._ // for tell

    type Logged[A] = Writer[Vector[String], A]

    def factorial(n: Int): Logged[Int] = {
      val a: Logged[Int] = {
        if (n == 0)
          1.pure[Logged]
        else
          factorial(n - 1).map(x1 => x1 * n)
      }

      val b: WriterT[Id, Vector[String], Int] = a.flatMap(ans => Vector(s"fact $n $ans").tell.map(_ => ans))
      b
    }

//    def factorial(n: Int): Logged[Int] =
//      for {
//        ans <- if(n == 0) {
//          1.pure[Logged]
//        } else {
//          factorial(n - 1).map(_ * n)
//        }
//        _ <- Vector(s"fact $n $ans").tell
//      } yield ans

    val (log, res) = factorial(5).run

    assert(log == Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"))
    assert(res == 120)

    val Vector((logA, ansA), (logB, ansB)) =
      Await.result(Future.sequence(Vector(Future(factorial(3).run), Future(factorial(5).run))), Duration.Inf)

    assert(logA == Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"))
    assert(logB == Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"))
    assert(ansA == 6)
    assert(ansB == 120)

  }
}
