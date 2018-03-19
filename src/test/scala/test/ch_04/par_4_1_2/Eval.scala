package test.ch_04.par_4_6_2

import org.scalatest.FunSuite

class Eval extends FunSuite {
  /*
    Scala       Cats      Properties
    val         Now       eager, memoized
    lazy val    Later     lazy, momoized
    def         Always    lazy, not momoized
  * */
  test("eval2") {
    import cats.Eval

    {
      var tot = 0

      val x = Eval.now {
        println("Eval.now like val eager and memoized")
        tot += 1
      }

      assert(tot == 1)

      val p1 = x.value
      assert(tot == 1)

      val p2 = x.value
      assert(tot == 1)
    }

    {
      var tot = 0

      val x = Eval.always {
        println("Eval.always like def")
        tot += 1
      }

      val p1 = x.value
      assert(tot == 1)

      val p2 = x.value
      assert(tot == 2)

      val p3 = x.value
      assert(tot == 3)
    }
    {
      var tot = 0
      val x = Eval.later {
        println("Eval.later like lazy val")
        tot += 1
      }

      assert(tot == 0)

      val p2 = x.value
      assert(tot == 1)

      val p3 = x.value
      assert(tot == 1)
    }

  }

  test("memoize") {
    import cats.Eval
    var tot = 0
    val m = Eval.now {
      tot += 1
      math.random
    }.memoize

    val p1 = m
    assert(tot == 1)
    val p2 = m
    assert(tot == 1)
  }


}