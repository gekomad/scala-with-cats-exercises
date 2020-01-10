package test.ch_04.par_4_10_1

import org.scalatest.FunSuite

class CustomMonadTest extends FunSuite {

  test("custom monad") {
    import cats.Monad

    import scala.annotation.tailrec

    val optionMonad = new Monad[Option] {
      def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] = opt flatMap fn

      def pure[A](opt: A): Option[A] = Some(opt)

      @tailrec
      def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] = fn(a) match {
        case None           => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
    }
  }

  test("tree monad") {
    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A)                        extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)

    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

      def leaf[A](value: A): Tree[A] = Leaf(value)
    }

    import cats.Monad

    implicit val treeMonad = new Monad[Tree] {
      override def flatMap[A, B](fa: Tree[A])(fn: A => Tree[B]): Tree[B] = fa match {
        case Branch(a, b) =>
          Branch(flatMap(a)(fn), flatMap(b)(fn))
        case Leaf(a) =>
          fn(a)
      }

      def pure[A](opt: A): Tree[A] = Leaf(opt)

      def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] =
        func(arg) match {
          case Branch(l, r) =>
            Branch(flatMap(l) {
              case Left(l)  => tailRecM(l)(func)
              case Right(l) => pure(l)
            }, flatMap(r) {
              case Left(r)  => tailRecM(r)(func)
              case Right(r) => pure(r)
            })
          case Leaf(Left(value))  => tailRecM(value)(func)
          case Leaf(Right(value)) => Leaf(value)

        }
    }

    import cats.syntax.flatMap._ // for flatMap
    val p = branch(branch(leaf(1), leaf(2)), leaf(3)).flatMap(a => Leaf(1))
    assert(p == branch(branch(leaf(1), leaf(1)), leaf(1)))

    val p2 = branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
    assert(p2 == branch(branch(leaf(99), leaf(101)), branch(leaf(199), leaf(201))))
  }
}
