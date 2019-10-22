package exercise

abstract class XCollection[A] {
  def head: Option[A]
  def tail: Option[XCollection[A]]
  def isEmpty: Boolean
  def add(x: A): XCollection[A]

  def printElements: String

  // polymorphic call
  override def toString = s"[${printElements}]"
}

case class Empty[A]() extends XCollection[A] {
  def head = None
   // throw new NoSuchElementException
  def tail= None
  def isEmpty = true
  def add(x: A) = Cons(x, Empty[A])

  def printElements = ""
}

case class Cons[A](h: A, t: XCollection[A]) extends XCollection[A] {
  def head = Some(h)
  def tail = Some(t)
  def isEmpty = false
  def add(x: A): XCollection[A] = Cons(x, Cons(h, t))

  def printElements:String = {
    if (t.isEmpty) h.toString
    else s"$h ${t.printElements}"
  }
}

object Test extends App {
  val myList = Cons(1, Cons(4, Cons(5, Empty())))

  println(myList.toString)

  (myList head) map(x => x + 1)
}