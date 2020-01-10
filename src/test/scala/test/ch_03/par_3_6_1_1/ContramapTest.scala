package test.ch_03.par_3_6_1_1

import org.scalatest.FunSuite

class ContramapTest extends FunSuite {

  test("contramap") {

    trait Printable[A] {
      self =>
      def format(value: A): String

      def contramap[B](func: B => A): Printable[B] =
        (value: B) => {
          val p = self.format(func(value))
          p
        }
    }

    def format[A](value: A)(implicit p: Printable[A]): String = "*" + p.format(value) + "*"

    implicit val stringPrintable: Printable[String] =
      (value: String) => s"""($value)"""

    implicit val booleanPrintable: Printable[Boolean] =
      (value: Boolean) => if (value) "yes" else "no"

    val s = format("hello")
    assert(s == s"""*(hello)*""")

    val s2 = format(true)
    assert(s2 == "*yes*")

  }
}
