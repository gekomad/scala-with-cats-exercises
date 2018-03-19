package test.ch_06.par_7_1

import org.scalatest.FunSuite

class FoldableTest extends FunSuite {

  test("7.1.2 Exercise: Reflec ng on Folds") {

    val ll = List(1, 2, 3).foldLeft(List[Int]())((accum, item) => item :: accum)

    val lr = List(1, 2, 3).foldRight(List[Int]())((item, accum) => item :: accum)

    assert(ll == List(3, 2, 1))

    assert(lr == List(1, 2, 3))

  }

  test("7.1.3 Exercise: Scaffolding Other Methods") {

    import scala.collection.immutable

    def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
      list.foldRight(List.empty[B]) { (item, accum) =>
        func(item) ::: accum
      }

    val map1 = List(1, 2, 3).foldRight(List[Int]())((item, accum) => item + 1 :: accum)
    assert(map1 == List(1, 2, 3).map(_ + 1))

    val flatMap1: immutable.Seq[Int] = List(1, 2, 3).foldRight(List[Int]())((item, accum) => (item + 1) :: accum)
    assert(flatMap1 == List(1, 2, 3).flatMap(a => List(a + 1)))

    val filter1 = List(1, 2, 3, 4).foldRight(List[Int]())((item, accum) => if (item % 2 == 0) item :: accum else accum)
    assert(filter1 == List(1, 2, 3, 4).filter(_ % 2 == 0))

    val sum1 = List(1, 2, 3).foldRight(0)((item, accum) => item + accum)
    assert(sum1 == List(1, 2, 3).sum)
  }

  test("option") {
    import cats.Foldable
    import cats.instances.option._

    {
      val maybeInt = None: Option[Int]
      val d = Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
      assert(10 == d)
    }

    {
      val maybeInt = Option(123)
      val d = Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
      assert(1230 == d)
    }

  }

  test("traverse") {
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com"
    )

    def getUptime(hostname: String): Future[Int] =
      Future(hostname.length * 60) // just for demonstration


    val allUptimes: Future[List[Int]] = hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime: Future[Int] = getUptime(host)
        for {
          accum <- accum
          uptime <- uptime
        } yield accum :+ uptime
    }

    val o = Await.result(allUptimes, 1.second)
    val allUptimes2: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
    val s = Await.result(allUptimes2, 1.second)
    assert(o == s)
  }

  test("serialiseFutures") {

    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val list = List(1, 2, 3, 4, 5, 6)
    val listPlusOne = list.map(_ + 1)
    val sleep = 100

    def serialiseFutures[A, B](l: Iterable[A])(fn: A => Future[B]): Future[List[B]] =
      l.foldLeft(Future(List.empty[B])) {
        (previousFuture, next) =>
          for {
            previousResults <- previousFuture
            next <- fn(next)
          } yield previousResults :+ next
      }


    def t1 = serialiseFutures(list) { i =>
      Future {
        Thread.sleep(sleep)
        i + 1
      }
    }


    {
      val start = System.currentTimeMillis

      val r1 = t1
      val r2 = t1
      val r3 = t1

      val d1 = Await.result(r1, Duration.Inf)
      val d2 = Await.result(r2, Duration.Inf)
      val d3 = Await.result(r3, Duration.Inf)

      val timeElapsed = System.currentTimeMillis - start

      assert(listPlusOne == d1)
      assert(listPlusOne == d2)
      assert(listPlusOne == d3)
      assert(timeElapsed > list.size * sleep && timeElapsed < (list.size + 1) * sleep)
    }


  }

}