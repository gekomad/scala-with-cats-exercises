package test.ch_01.par_1_2

import java.time.LocalDateTime

import cats.Eq
import cats.instances.all._
import cats.syntax.eq._
import cats.syntax.option._

import org.scalatest.{FunSuite, NonImplicitAssertions}

class EqualityTest extends FunSuite with NonImplicitAssertions {

  test("Equality") {

    val o: Option[Int] = Some(1)
    val o2: Option[Int] = Some(1)
    assert(o =!= (None: Option[Int]))
    assert(o === o2)

    assert(1.some =!= None)
    assert("aaa" === "aaa")

    assert(Option(1) =!= Option.empty[Int])

  }

  test("Custom types") {

    implicit val localdtEqual = Eq.instance[LocalDateTime] { (l1, l2) => l1 == l2 }
    final case class Cat(name: String, age: Int, color: String, datetime: LocalDateTime)


    implicit val catEqual = Eq.instance[Cat] { (cat1, cat2) =>
      import cats.instances.int._
      import cats.instances.string._
      (cat1.name === cat2.name) &&
        (cat1.datetime === cat2.datetime) &&
        (cat1.age === cat2.age) &&
        (cat1.color === cat2.color)
    }

    val now = LocalDateTime.now
    val cat1 = Cat("Garfield", 35, "orange and black", now)

    val cat2 = Cat("Heathcliff", 30, "orange and black", now)

    assert(cat1 =!= cat2)

    {
      val cat2bis = cat2.copy()
      assert(cat2bis === cat2)

      val cat2tris = cat2.copy(datetime = LocalDateTime.MAX)
      assert(cat2tris =!= cat2)
    }


    import cats.instances.option._
    val optionCat1 = Option(cat1)

    val optionCat2 = Option.empty[Cat]

    assert(!(optionCat1 === optionCat2))
    assert(optionCat1 =!= optionCat2)


  }
}