package test.ch_06.par_6_4_4

import org.scalatest.FunSuite

class FormValidationTest extends FunSuite {

  test("form validation Either") {
    import scala.util.{Failure, Success, Try}
    type FormData    = Map[String, String]
    type FailFast[A] = Either[List[String], A]

    def getValue(m: Map[String, String], n: String): FailFast[String] = {
      val x = m.get(n)
      val o = x.map(c => Right(c)).getOrElse(Left(List(n)))
      o
    }

    def parseInt(s: String): FailFast[Int] = Try(s.toInt) match {
      case Success(r) => Right(r)
      case Failure(_) => Left(List(s))
    }

    def nonNegative(s: Int): FailFast[Int] = if (s > 0) Right(s) else Left(List(s.toString))

    def nonBlank(n: String): FailFast[String] = {
      val o = if (n.length > 0) Right(n) else Left(List(n))
      o
    }

    def readName(m: Map[String, String]): FailFast[String] = {
      val n = for {
        n  <- getValue(m, "name")
        nb <- nonBlank(n)
      } yield nb

      n
    }

    def readAge(m: Map[String, String]): FailFast[Int] = {
      val a = for {
        n  <- getValue(m, "age")
        i  <- parseInt(n)
        nb <- nonNegative(i)
      } yield nb
      a
    }

    val ageMap = Map("age" -> "-1", "age" -> "2")
    assert(readAge(ageMap) == Right(2))

    val nameMap = Map("name" -> "pippo", "name" -> "")
    assert(readName(nameMap) == Left(List("")))

  }

  object EitherOnly {

    import cats.syntax.either._ // for catchOnly
    type FormData    = Map[String, String]
    type FailFast[A] = Either[List[String], A]

    def getValue(data: Map[String, String], name: String): FailFast[String] = {
      val x: Option[String] = data.get(name)
      x.toRight(List(s"$name field not specified"))

    }

    def parseInt(data: String): FailFast[Int] = {
      val x: Either[NumberFormatException, Int] = Either.catchOnly[NumberFormatException](data.toInt)
      x.leftMap(e => List(s"must be an integer $e"))
    }

    assert(parseInt("42") == Right(42))
    assert(
      parseInt("foo") == Left(List(s"""must be an integer java.lang.NumberFormatException: For input string: "foo""""))
    )

    def nonNegative(data: Int): FailFast[Int] =
      Right[List[String], Int](data).ensure(List(s"must be non-negative"))(_ >= 0)

    def nonBlank(data: String): FailFast[String] = {
      val x: FailFast[String] = Right(data)
      val y: FailFast[String] = x.ensure(List(s"cannot be blank"))(_.nonEmpty)
      y
    }

    def readName(m: Map[String, String]): FailFast[String] =
      for {
        n  <- getValue(m, "name")
        nb <- nonBlank(n)
      } yield nb

    def readAge(m: Map[String, String]): FailFast[Int] =
      for {
        n  <- getValue(m, "age")
        i  <- parseInt(n)
        nb <- nonNegative(i)
      } yield nb

  }

  test("form validation Validation") {
    final case class User(name: String, age: Int)
    import cats.data.Validated
    import cats.instances.list._ // for Semigroupal
    import cats.syntax.apply._   // for mapN
    import cats.syntax.either._  // for catchOnly
    import EitherOnly._

    type FailSlow[A] = Validated[List[String], A]

    {
      val webForm                  = Map("age" -> "-1", "age" -> "2", "name" -> "pippo", "name" -> "")
      val ageEither: FailFast[Int] = readAge(webForm)
      assert(ageEither == Right(2))

      val ageValidated = Validated.fromEither[List[String], Int](ageEither)

      val nameEither: FailFast[String] = readName(webForm)
      assert(nameEither == Left(List("cannot be blank")))
      val nameValidated: Validated[List[String], String] = Validated.fromEither[List[String], String](nameEither)
    }

    def readUser(data: FormData): FailSlow[User] = {
      val o1: Validated[List[String], String] = readName(data).toValidated
      val o2: Validated[List[String], Int]    = readAge(data).toValidated

      (o1, o2).mapN(User.apply)
    }

    val l: FailSlow[User] = readUser(Map("name" -> "Dave", "age" -> "37"))
    assert(l == Validated.Valid(User("Dave", 37)))

    val l2: FailSlow[User] = readUser(Map("age" -> "-1"))
    assert(l2 == Validated.Invalid(List("name field not specified", "must be non-negative")))

    val l3 = readUser(Map("name" -> "Dave", "age" -> "-1"))
    assert(l3 == Validated.Invalid(List("must be non-negative")))

  }

}
