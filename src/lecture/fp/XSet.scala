package lecture.fp

import scala.annotation.tailrec

sealed trait XSet[A] extends (A => Boolean) {
  override def apply(v1: A): Boolean = contains(v1)

  def contains(elem: A): Boolean
  def +(elem: A): XSet[A]
  def -(elem: A): XSet[A]
  def ++(set: XSet[A]): XSet[A] // union
  def --(set: XSet[A]): XSet[A] // difference
  def &(set: XSet[A]): XSet[A] // intersection
  def map[B](f: A => B): XSet[B]
  def flatMap[B](f: A => XSet[B]): XSet[B]
  def filter(p: A => Boolean): XSet[A]
  def foreach(f: A => Unit): Unit
  def unary_! : XSet[A]
}

// set of A that satisfies given property
final class PropertyBasedSet[A](prop: A => Boolean) extends XSet[A] {
  def contains(elem: A): Boolean = prop(elem)

  def +(elem: A): XSet[A] =
    new PropertyBasedSet[A](x => prop(x) || x == elem)

  def -(elem: A): XSet[A] = filter(_ != elem)
  def ++(set: XSet[A]): XSet[A] =
    new PropertyBasedSet[A](x => prop(x) || set(x))

  def --(set: XSet[A]): XSet[A] = filter(!set)
  def &(set: XSet[A]): XSet[A] = filter(set)
  def map[B](f: A => B): XSet[B] = fail
  def flatMap[B](f: A => XSet[B]): XSet[B] = fail
  // property based set
  def filter(p: A => Boolean): XSet[A] =
    new PropertyBasedSet[A](x => prop(x) && p(x))

  def foreach(f: A => Unit): Unit = fail
  def unary_! : XSet[A] =
    new PropertyBasedSet[A](!prop(_))

  private def fail = throw new IllegalArgumentException
}

final class EmptySet[A] extends XSet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): XSet[A] = new NonEmptySet[A](elem, this)
  def ++(set: XSet[A]): XSet[A] = set
  def map[B](f: A => B): XSet[B] = new EmptySet[B]
  def flatMap[B](f: A => XSet[B]) = new EmptySet[B]
  def filter(p: A => Boolean): XSet[A] = this
  def foreach(f: A => Unit): Unit = ()
  def -(elem: A): XSet[A] = this
  def --(set: XSet[A]): XSet[A] = this
  def &(set: XSet[A]): XSet[A] = this
  def unary_! : XSet[A] = new PropertyBasedSet[A](_ => true)
}

final case class NonEmptySet[A](head: A, tail: XSet[A]) extends XSet[A] {
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  def +(elem: A): XSet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)

  def ++(set: XSet[A]): XSet[A] =
    tail ++ set + head

  def map[B](f: A => B): XSet[B] =
    (tail map f) + f(head)


  def flatMap[B](f: A => XSet[B]): XSet[B] =
    (tail flatMap f) ++ f(head)

  def filter(p: A => Boolean): XSet[A] = {
      val filteredTail = tail filter p
      if (p(head)) filteredTail + head
      else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach(f)
  }

  def unary_! : XSet[A] =
    new PropertyBasedSet[A](!this.contains(_))

  def -(elem: A): XSet[A] =
    if (head == elem) tail
    else tail - elem + head

  def --(set: XSet[A]): XSet[A] = filter(!set)
  def &(set: XSet[A]): XSet[A] = filter(set)
}

object XSet {
  def apply[A](values: A*): XSet[A] =
    values.toSeq.foldLeft[XSet[A]](new EmptySet[A])(_ + _)
}

object Playground extends App {
  val x = XSet(1, 2, 3, 4)

  x + 5 + 4 foreach println
}