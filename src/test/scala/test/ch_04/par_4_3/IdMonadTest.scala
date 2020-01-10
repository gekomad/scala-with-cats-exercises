package test.ch_04.par_4_3

import org.scalatest.FunSuite

class IdMonadTest extends FunSuite {

  test("identity") {

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    import cats.Monad

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      for {
        x <- a
        y <- b
      } yield x * x + y * y

    import cats.Id

    val o = sumSquare(3: Id[Int], 4: Id[Int])
    assert(o == 25)
  }

}
