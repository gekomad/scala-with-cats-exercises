package test.ch_08.par_8_1

import cats.instances.all._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import org.scalatest.FunSuite
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Future, _}

class AbstractTypeConstructorsTest extends FunSuite {

  def fn(hosts: Map[String, Int], hostname: String) = hosts.getOrElse(hostname, 0)

  val map = Map("a" -> 1, "b" -> 2)

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int] = Future(fn(hosts, hostname))
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int = fn(hosts, hostname)
  }

  test("8.1") {

    // test
    assert((new TestUptimeClient(map)).getUptime("a") == 1)

    // prod
    val r: Future[Int] = (new RealUptimeClient(map)).getUptime("a")
    assert(Await.result(r, Duration.Inf) == 1)
  }

  test("8.2") {

    class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
      def getTotalUptime(hostnames: List[String]): F[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
    }

    // test
    {

      val s = new TestUptimeClient(map)

      val r1 = (new UptimeService[Id](s)).getTotalUptime(List("b", "a", "a"))
      assert(r1 == 4)
    }

    // prod
    {

      val s  = new RealUptimeClient(map)
      val r2 = (new UptimeService[Future](s)).getTotalUptime(List("b", "a", "a"))
      assert(Await.result(r2, Duration.Inf) == 4)
    }
  }
}
