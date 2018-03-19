package test.ch_09.par_9_2


import java.util.concurrent.ForkJoinTask

import test.utility.Utility._
import org.scalameter.{Key, Warmer, config}
import org.scalatest.FunSuite

class FoldMapTest extends FunSuite {

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.semigroup._ // for Monoid

  def foldMap[A, B: Monoid](as: Vector[A])(func: A => B): B =
    as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  test("foldMap") {

    assert(foldMap(Vector(1, 2, 3))(identity) == 6)

    val t = foldMap(Vector(1, 2, 3))(_.toString + "! ")
    assert(t == "1! 2! 3! ")

    val a = foldMap("Hello world!".toVector)(_.toString.toUpperCase)
    assert(a == "HELLO WORLD!")

  }

  test("9.3.3") {
    import cats.Monoid

    import scala.concurrent.Future
    // for Monoid
    import cats.instances.future._
    import cats.instances.vector._
    import cats.syntax.foldable._
    import cats.syntax.semigroup._
    import cats.syntax.traverse._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    def parallelFoldMapSlow[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
      val batches = values.toList.grouped(Runtime.getRuntime.availableProcessors)
      val batchesPerCPU: Iterator[Future[B]] = batches.map(group => Future(foldMap(group.toVector)(func)))
      val reduce: Future[Iterator[B]] = Future.sequence(batchesPerCPU)
      val r = reduce.map(f => f.foldLeft(Monoid[B].empty)(_ |+| _))
      r
    }

    def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
      val groupSize = (1.0 * values.size / Runtime.getRuntime.availableProcessors).ceil.toInt
      val batches: Iterator[Vector[A]] = values.grouped(groupSize)
      val batchesPerCPU: Future[Vector[B]] = batches.toVector.traverse(group => Future(foldMap(group)(func)))
      //      val batchesPerCPU: Future[Vector[B]] = batches.toVector.traverse(group => Future(group.foldMap(func)))
      val l: Future[B] = batchesPerCPU.map(_.combineAll)
      l
    }


    val list = (1 to 1000).toVector
    var res = list.foldLeft("")(_ + _ + "!")

    assert(Await.result(parallelFoldMap(list)((a) => a + "!"), Duration.Inf) == res)
    assert(Await.result(parallelFoldMapSlow(list)((a) => a + "!"), Duration.Inf) == res)

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 50,
      Key.exec.maxWarmupRuns -> 100,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false
    ) withWarmer new Warmer.Default

    {
      val time = standardConfig measure {

        def r = serialiseFutures( (1 to 100)) { _ =>
          parallelFoldMap(list)((a) => a + "!")
        }

        Await.result(r, Duration.Inf)
      }

      println(s"parallelFoldMap: $time")

    }

    {
      val time = standardConfig measure {

        def r = serialiseFutures( (1 to 100)) { _ =>
          parallelFoldMapSlow(list)((a) => a + "!")
        }

        Await.result(r, Duration.Inf)
      }

      println(s"parallelFoldMapSlow: $time")

    }
  }

}