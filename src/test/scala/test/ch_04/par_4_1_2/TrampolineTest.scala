package test.ch_04.par_4_1_2

import cats.Eval
import org.scalameter.{Key, Warmer, config}
import org.scalatest.FunSuite

/*
  Scala       Cats      Properties
  val         Now       eager, memoized
  lazy val    Later     lazy, momoized
  def         Always    lazy, not momoized
* */

object Fact {
  def factorialIterative(n: BigInt): BigInt = {
    var i = n
    var a: BigInt = 1
    while (i > 0) {
      a = a * i
      i -= 1
    }
    a
  }

  def factorialTrampoline(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorialTrampoline(n - 1).map(_ * n))
    }
}

class TrampolineTest extends FunSuite {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 100,
    Key.exec.benchRuns -> 100,
    Key.verbose -> false
  ) withWarmer new Warmer.Default

  ignore("Safer Folding using Eval") {

    val N = 500000

    lazy val classic = {
      var res = 0L
      val time = standardConfig measure {
        res = (1 to N).toList.foldRight(0l)((a, b) => a + b)
      }
      (res, time)
    }

    lazy val trampoline1 = {
      var res = 0L

      def foldRightSafer[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
        case head :: tail =>
          Eval.defer(foldRightSafer(tail, acc)(fn)).map(a => fn(head, a))
        case Nil => Eval.now(acc)
      }

      val time = standardConfig measure {
        res = foldRightSafer((1 to N).toList, 0l)((a, b) => a + b).value
      }
      (res, time)
    }

    lazy val trampoline2 = {
      var res = 0L
      import cats.Eval
      def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
        as match {
          case head :: tail =>
            Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
          case Nil =>
            acc
        }

      def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
        foldRightEval(as, Eval.now(acc)) { (a, b) =>
          b.map(fn(a, _))
        }.value


      val time = {
        standardConfig measure {
          res = foldRight((1 to N).toList, 0L)(_ + _)
        }


      }
      (res, time)
    }

    println(s"** time foldRight classic: ${classic._2} **")
    println(s"** time foldRight trampoline1: ${trampoline1._2} **")
    println(s"** time foldRight trampoline2: ${trampoline2._2} **")
    assert(classic._1 == trampoline1._1)
    assert(classic._1 == trampoline2._1)
  }

  ignore("iterative/rescursive") {
    val n = 20000

    var res1: BigInt = 0

    println("iterative...")

    {

      val time = standardConfig measure {
        res1 = Fact.factorialIterative(n)
      }

      println(s"** time iterative: $time **")

    }

    println("recursive...")

    {
      var res2: BigInt = 0
      val time = standardConfig measure {
        res2 = Fact.factorialTrampoline(n).value
        // println(p)
      }

      println(s"** time rescursive: $time **")

      assert(res1 == res2)
    }
  }


}