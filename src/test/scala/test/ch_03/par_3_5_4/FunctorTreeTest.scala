package test.ch_03.par_3_5_4

import org.scalatest.FunSuite

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

class FunctorTreeTest extends FunSuite {

  test("Functor tree") {

    import cats.Functor
    import cats.syntax.functor._

    implicit val l3 = new Functor[Tree] {
      def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Branch(a, b) =>
          Branch(map(a)(f), map(b)(f))
        case Leaf(a) =>
          Leaf(f(a))
      }
    }
    import Tree._
    val m: Tree[Int] = branch(branch(leaf(1), leaf(2)), leaf(3))
    val p = m.map(_ * 2)
    assert(p == Branch(Branch(Leaf(2), Leaf(4)), Leaf(6)))

    val o: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val x = o.map(_ * 2)
    assert(x == Branch(Leaf(20), Leaf(40)))

    val p1: Tree[Int] = leaf(100).map(_ * 2)
    assert(p1 == Leaf(200))

    val p2 = branch(leaf(10), leaf(20)).map(_ * 2)
    assert(p2 == Branch(Leaf(20), Leaf(40)))
  }
}