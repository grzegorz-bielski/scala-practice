package exercise

abstract sealed class Maybe[+T] {
  def map[B](f: T => B): Maybe[B]
  def flatMap[B](f: T => Maybe[B]): Maybe[B]
  def filter(p: T => Boolean): Maybe[T]
}

case object None2 extends Maybe[Nothing] {
  def map[B](f: Nothing => B) = None2
  def flatMap[B](f: Nothing => Maybe[B]) = None2
  def filter(p: Nothing => Boolean) = None2
}

case class Just[+T](value: T) extends Maybe[T] {
  def map[B](f: T => B) = Just(f(value))
  def flatMap[B](f: T => Maybe[B]) = f(value)
  def filter(p: T => Boolean): Maybe[T] =
    if (p(value)) this else None2
}


