package test.ch_01.par_1_2

import java.time.LocalDateTime
import cats.Show
import cats.implicits._
import org.scalatest.FunSuite

class SyntaxCatsTest extends FunSuite {

  test("PrintableSyntax") {

    implicit val showDatetime = Show[LocalDateTime](input => input.toString)
    final case class Cat(name: String, age: Int, color: String, datetime: LocalDateTime)

    implicit val catShow = Show.show[Cat] { cat =>
      val name     = cat.name.show
      val age      = cat.age.show
      val color    = cat.color.show
      val datetime = cat.datetime.show
      s"$name is a $age year-old $color cat datetime $datetime"
    }

    val d: LocalDateTime = LocalDateTime.now

    val cat         = Cat("Garfield", 35, "ginger and black", d)
    val res: String = cat.show
    assert(res == s"Garfield is a 35 year-old ginger and black cat datetime ${d.show}")

  }
}
