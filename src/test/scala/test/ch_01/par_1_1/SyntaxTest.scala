package test.ch_01.par_1_1

import org.scalatest.FunSuite

trait Printable[A] {
  def format(value: A): String
}

object Printable {

  def theFormat[A](input: A)(implicit p: Printable[A]): String =
    p.format(input)

  def print[A](input: A)(implicit p: Printable[A]): Unit =
    println(theFormat(input))
}

import java.time.LocalDateTime

object PrintableInstances {

  implicit val stringPrintable = new Printable[String] {
    def format(input: String) = input
  }

  implicit val intPrintable = new Printable[Int] {
    def format(input: Int) = input.toString
  }

  implicit val datePrintable = new Printable[LocalDateTime] {
    def format(input: LocalDateTime) = input.toString
  }

}
final case class Cat(name: String, age: Int, color: String, datetime: LocalDateTime)

class TestSyntax extends FunSuite {

  object PrintableSyntax {

    implicit class PrintOps[A](value: A) {
      def format(implicit p: Printable[A]): String =
        p.format(value)

      def print(implicit p: Printable[A]): Unit =
        println(p.format(value))
    }

  }

  test("PrintableSyntax") {

    import PrintableSyntax._

    import Cat._

    import Printable._

    import PrintableInstances._

    implicit val catPrintable = new Printable[Cat] {
      def format(cat: Cat) = {
        val name  = Printable.theFormat(cat.name)
        val age   = Printable.theFormat(cat.age)
        val color = Printable.theFormat(cat.color)
        val d     = Printable.theFormat(cat.datetime)
        s"$name is a $age year-old $color cat. date $d"
      }
    }

    Cat("Garfield", 35, "ginger and black", LocalDateTime.now).print

  }
}
