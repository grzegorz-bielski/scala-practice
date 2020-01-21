package lecture.typesystem

object hkt extends App {

  // higher kinded types

  trait XXList[T] {
    def flatMap[B](f: T => B): XXList[B]
  }

//  class Functor f where
//    map :: (a -> b) -> f a -> f b

//  trait Functor[F[_], A] {
//    def map[B](f: A => F[B]): F[B]
//  }

  // higher kinded type class
  trait Monad[F[_], A] {
    def flatMap[B](f: A => F[B]): F[B]
    def map[B](f: A => B): F[B]
  }

  implicit class MonadList[A](list: List[A]) extends Monad[List, A] {
    override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)

    override def map[B](f: A => B): List[B] = list.map(f)
  }

  // ( we don't need type enrichments for map/flatmap here, they are implemented on the list/option

  def multiply[F[_], A, B](ma: Monad[F, A], mb: Monad[F, B]): F[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  // ma.flatMap(a => mb.map(b => (a,b)))

  multiply(new MonadList(List(1, 2, 4)), new MonadList(List(1, 2, 4)))
  val res = multiply(List(1, 2, 4), List(1, 2))
  // multiply(Option, Option)
}
