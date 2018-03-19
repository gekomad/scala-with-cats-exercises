package test.ch_04.par_4_9_3

import org.scalatest.FunSuite

class StateMonadTest extends FunSuite {

  test("state") {
    import cats.data.State

    val step1: State[Int, String] = State[Int, String] { num =>
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }

    val step2: State[Int, String] = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)

    val (state, result) = both.run(20).value
    assert(state == 42)
    assert(result == ("Result of step1: 21", "Result of step2: 42"))
  }

  test("get/set/modify/inspect") {
    import cats.data.State
    import cats.data.State._

    val program: State[Int, (Int, Int, Int)] = {
      val a = for {
        a <- get[Int]
        _ <- set[Int](a + 1)
        b <- get[Int]
        _ <- modify[Int](_ + 1)
        c <- inspect[Int, Int](_ * 1000)
      } yield (a, b, c)
      a
    }

    val (state, result) = program.run(1).value
    assert(state == 3)
    assert(result == (1, 2, 3000))
  }

  test("parser") {
    import cats.data.State

    type CalcState[A] = State[List[Int], A]

    //    def operatorxxxxxx(func: (Int, Int) => Int): CalcState[Int] =
    //      State[List[Int], Int] {
    //        case a :: b :: tail =>
    //          val ans = func(a, b)
    //          (ans :: tail, ans)
    //        case _ =>
    //          sys.error("Fail!")
    //      }

    def evalOne(sym: String): State[List[Int], Int] = sym match {
      case "+" => State[List[Int], Int] { s =>
        (s.head + s.tail.head :: s.tail, s.head + s.tail.head)
      }
      case "-" => State[List[Int], Int] { s =>
        (s.head - s.tail.head :: s.tail, s.head - s.tail.head)
      }
      case "*" => State[List[Int], Int] { s =>
        (s.head * s.tail.head :: s.tail, s.head * s.tail.head)
      }
      case "/" => State[List[Int], Int] { s =>
        (s.head / s.tail.head :: s.tail, s.head / s.tail.head)
      }
      case v => State[List[Int], Int] { s => (v.toInt :: s, v.toInt) }
    }

    assert(42 == evalOne("42").runA(Nil).value)

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    assert(program.run(Nil).value == (List(3, 1), 3))

    import cats.syntax.applicative._ // for pure

    def evalAll(input: List[String]): State[List[Int], Int] = {
      input.foldLeft(0.pure[CalcState]) { (a, t) =>
        a.flatMap(_ => evalOne(t))
      }
    }

    assert(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value == 9)
    assert(evalAll(List()).runA(Nil).value == 0)

    val program3 = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    assert(program3.run(Nil).value == (List(21, 3, 3, 1), 21))
  }
}