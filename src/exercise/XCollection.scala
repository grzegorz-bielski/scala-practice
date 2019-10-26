package exercise

abstract class XCollection[+A] {
  def head: Option[A]
  def tail: Option[XCollection[A]]
  def isEmpty: Boolean
  def add[B >: A](x: B): XCollection[B]

  def printElements: String

  // polymorphic call
  override def toString = s"[${printElements}]"

  def map[B](f: Transformer[A, B]): XCollection[B]
  def flatMap[B](f: Transformer[A, XCollection[B]]): XCollection[B]
  def filter(p: Predicate[A]): XCollection[A]

  def <>[B >: A](l: XCollection[B]): XCollection[B]
}

case class Empty() extends XCollection[Nothing] {
  def head = None
  def tail= None
  def isEmpty = true
  def add[B >: Nothing](x: B) = Cons(x, Empty())

  def printElements = ""

  def map[B](f: Transformer[Nothing, B]) = Empty()
  def flatMap[B](f: Transformer[Nothing, XCollection[B]]) = Empty()
  def filter(p: Predicate[Nothing]) = Empty()

  def <>[B >: Nothing](l: XCollection[B]): XCollection[B] = l
}

case class Cons[+A](h: A, t: XCollection[A]) extends XCollection[A] {
  def head = Some(h)
  def tail = Some(t)
  def isEmpty = false
  def add[B >: A](x: B): XCollection[B] = Cons(x, Cons(h, t))

  def printElements:String = {
    if (t.isEmpty) h.toString
    else s"$h ${t.printElements}"
  }

//  def map[B](f: Transformer[A, B]): XCollection[B]
  def flatMap[B](tr: Transformer[A, XCollection[B]]): XCollection[B] = {
    tr.transform(h) <> t.flatMap(tr)
  }

  def filter(p: Predicate[A]): XCollection[A] = {
      if (p.test(h)) Cons(h, t.filter(p))
      else t.filter(p)
  }

  def map[B](tr: Transformer[A, B]): XCollection[B] = {
    Cons(tr.transform(h), t.map(tr))
  }

  def <>[B >: A](l: XCollection[B]): XCollection[B] = {
    Cons(h, t <> l)
  }
}

trait Predicate[-T] {
  def test(a: T): Boolean
}
trait Transformer[-A, B] {
  def transform(a: A): B
}


object Test extends App {
  val myList = Cons(1, Cons(4, Cons(5, Empty())))

//  println(myList.toString)

  val mapped = myList.map(new Transformer[Int, Int] {
    override def transform(a: Int): Int = a * 2
  })

  val filtered = myList.filter(new Predicate[Int] {
    override def test(a: Int): Boolean = a % 2 == 0
  })

  filtered.flatMap(new Transformer[Int, XCollection[Int]] {
    override def transform(a: Int): XCollection[Int] = Cons(a, Cons(a + 1, Empty()))
  })


  (myList head) map(x => x + 1)
}

