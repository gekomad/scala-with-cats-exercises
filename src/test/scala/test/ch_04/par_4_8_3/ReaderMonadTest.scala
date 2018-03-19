package test.ch_04.par_4_8_3

import org.scalatest.FunSuite

class ReaderMonadTest extends FunSuite {

  test("Hacking on Readers") {
    import cats.data.Reader
    import cats.syntax.applicative._ // for pure
    final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

    type DbReader[A] = Reader[Db, A] // -> Kleisli[M[_], C, B] ->  ReaderT[Id, C, B] -> C => Id[B] -> Db => Id[A]

    def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
      username <- findUsername(userId)
      passwordOk <- username.map(u => checkPassword(u, password)).getOrElse(false.pure[DbReader])
    } yield passwordOk


    val db = Db(usernames = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    ), passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"))

    assert(checkLogin(1, "zerocool").run(db) == true)

    assert(checkLogin(4, "davinci").run(db) == false)

  }

}