package test.utility

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Utility {

  def serialiseFutures[A, B](l: Iterable[A])(fn: A => Future[B]): Future[List[B]] =
    l.foldLeft(Future(List.empty[B])) {
      (previousFuture, next) =>
        for {
          previousResults <- previousFuture
          next <- fn(next)
        } yield previousResults :+ next
    }
}
