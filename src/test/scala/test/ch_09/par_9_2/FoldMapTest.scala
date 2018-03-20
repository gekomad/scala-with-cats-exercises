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


  test("foldMap") {

    def foldMap[A, B: Monoid](as: Vector[A])(func: A => B): B =
      as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

    assert(foldMap(Vector(1, 2, 3))(identity) == 6)

    val t = foldMap(Vector(1, 2, 3))(_.toString + "! ")
    assert(t == "1! 2! 3! ")

    val a = foldMap("Hello world!".toVector)(_.toString.toUpperCase)
    assert(a == "HELLO WORLD!")

  }

  test("9.3.3") {
    import cats.Monoid

    import scala.concurrent.Future // for Monoid
    import cats.instances.future._
    import cats.instances.vector._
    import cats.syntax.foldable._

    import cats.syntax.traverse._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
      val groupSize = (1.0 * values.size / Runtime.getRuntime.availableProcessors).ceil.toInt
      val batches: Iterator[Vector[A]] = values.grouped(groupSize)
      val batchesPerCPU: Future[Vector[B]] = batches.toVector.traverse(group => Future(group.foldMap(func)))
      val l: Future[B] = batchesPerCPU.map(_.combineAll)
      l
    }

    val list = (1 to 1000).toVector
    
    var res = list.foldLeft("")(_ + _ + "!")

    assert(Await.result(parallelFoldMap(list)((a) => a + "!"), Duration.Inf) == res)

  }

}