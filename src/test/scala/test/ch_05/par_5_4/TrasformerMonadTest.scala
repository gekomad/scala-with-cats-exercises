package test.ch_05.par_5_4

import org.scalatest.FunSuite
import scala.util.Try

class TrasformerMonadTest extends FunSuite {

  test("trasformer monad List[Option[A]]") {
    /*
    The combined map and flatMap methods allow us to use both component monads without having to recursively
    unpack and repack values at each stage in the computa on. Now let’s look at the API in more depth.
     * */
    import cats.data.OptionT
    type ListOption[A] = OptionT[List, A]

    import cats.instances.list._
    import cats.syntax.applicative._ // for pure

    val result1: ListOption[Int] = OptionT(List(Option(10)))
    //    val result1: ListOption[Int] = OptionT(List(None:Option[Int]))
    //    val result1: ListOption[Int] = OptionT(List(None:Option[Int]))

    val result2: ListOption[Int] = 32.pure[ListOption]

    val l: OptionT[List, Int] = result1.flatMap { x =>
      result2.map { y =>
        x + y
      }
    }

    assert(l.value == List(Some(42)))

  }

  test("trasformer monad2") {

    import cats.data.{EitherT, OptionT}
    import cats.instances.future._
    import cats.syntax.applicative._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._

    type FutureEither[A]       = EitherT[Future, String, A] // Future[Either[String,A]]
    type FutureEitherOption[A] = OptionT[FutureEither, A]   // Future[Either[String,Option[A]]]

    //type FutureEitherOption[A] = OptionT[EitherT[Future, String, A], A]

    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b

    val intermediate: FutureEither[Option[Int]]    = futureEitherOr.value
    val stack: Future[Either[String, Option[Int]]] = intermediate.value
    val dd: Either[String, Option[Int]]            = Await.result(stack, 1.second)
    val ef                                         = dd.getOrElse("s")
    assert(ef == Some(42))
    val s: Future[Either[String, Option[Int]]] = futureEitherOr.value.value
    val d                                      = s.map(r => r.getOrElse(None))

    val o: Option[Int] = Await.result(d, Duration.Inf)
    assert(o == Some(42))
  }

  test("trasformer 2") {
    import cats.data.Writer
    import cats.implicits._
    import cats.data.OptionT

    type Logged[A] = Writer[List[String], A]

    def parseNumber(str: String): Logged[Option[Int]] =
      Try(str.toInt).toOption match {
        case Some(num) => Writer(List(s"Read $str"), Some(num))
        case None      => Writer(List(s"Failed on $str"), None)
      }

    // Consumers use monad transformers locally to simplify composition:
    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {

      val result = for {
        a <- OptionT(parseNumber(a))
        b <- OptionT(parseNumber(b))
        c <- OptionT(parseNumber(c))
      } yield a + b + c
      result.value
    }

    val result1 = addAll("1", "2", "3").map(r => r).value

    assert(result1 == Some(6))
    val result2 = addAll("1", "a", "3").map(r => r).value
    assert(result2 == None)

  }

  test("Transform and Roll Out") {
    import cats.data.{EitherT}
    import cats.instances.future._
    import cats.syntax.applicative._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._

    //    type Response[A] = Future[Either[String, A]]
    type Response[A] = EitherT[Future, String, A]

    def getPowerLevel(autobot: String): Response[Int] = {
      val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

      val o: Option[Int] = powerLevels.get(autobot)

      val x = o.map(f => Right(f)).getOrElse(Left(s"err $autobot"))
      val s = EitherT.fromEither(x)

      s

    }

    val l = List("Jazz", "cop")

    val r: Seq[Response[Int]] = l.map(ms => getPowerLevel(ms))

    val ss = r.map(m => m.value)

    val sp = Future.sequence(ss)
    val r1 = Await.result(sp, Duration.Inf)

    assert(r1 == List(Right(6), Left("err cop")))
  }

  test("Transform and Roll Out2") {
    import cats.data.{EitherT}
    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._

    type Response[A] = EitherT[Future, String, A] // Future[Either[String, A]]

    val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

    def getPowerLevel(ally: String): Response[Int] = powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None      => EitherT.left(Future(s"$ally unreachable"))
    }

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
      for {
        power1 <- getPowerLevel(ally1)
        power2 <- getPowerLevel(ally2)
      } yield (power1 + power2) > 15

    def tacticalReport(ally1: String, ally2: String): String = {
      val stack = canSpecialMove(ally1, ally2).value
      Await.result(stack, 1.second) match {
        case Left(msg)    => s"Comms error: $msg"
        case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
        case Right(false) => s"$ally1 and $ally2 need a recharge."
      }
    }

    val o = tacticalReport("Jazz", "Bumblebee")

    assert(o == "Jazz and Bumblebee need a recharge.")
  }
}
