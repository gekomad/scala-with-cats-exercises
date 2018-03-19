package test.ch_04.par_4_7_3

import cats.Eval

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
