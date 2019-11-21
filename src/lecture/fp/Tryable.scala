package lecture.fp

import scala.util.{Try, Success, Failure}

object Tryable {
  //  sealed abstract class Try[+T]
  //  case class Failure[+T](t: Throwable) extends Try[T]
  //  case class Success[+T](value: T) extends Try[T]
  val aSuc = Success(3)
  val aFail = Failure(new RuntimeException("KEK"))

  def unsafe(): String = throw new RuntimeException("KEK")

  Try(unsafe()).map(x => x + 1)

  val result = Try {
    // sth throwable
  }

}
