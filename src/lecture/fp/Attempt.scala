package lecture.fp

trait Attempt[+A] {
  def flatMap[B](f: A => Attempt[B]): Attempt[B]
}

object Attempt {
  def apply[A](a: => A): Attempt[A] =
    try {
      SuccessA(a)
    } catch {
      case e: Throwable => Fail(e)
    }
}

case class SuccessA[+A](value: A) extends Attempt[A] {
  def flatMap[B](f: A => Attempt[B]): Attempt[B] =
    try {
      f(value)
    } catch {
      case e: Throwable => Fail(e)
    }

  def map[B](f: A => B): Attempt[B] =
    flatMap(a => SuccessA(f(a)))

  def flatten[B >: A](a: Attempt[B]): Attempt[B] =
    flatMap(_ => a)
}

case class Fail(e: Throwable) extends Attempt[Nothing] {
  def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  def map[B](f: Throwable => B): Attempt[B] = this
  def flatten[B >: Throwable](a: Attempt[B]): Attempt[B] = a
}


