package exercise

import scala.annotation.tailrec

abstract class XCollection[+A] {
  def head: Option[A]
  def tail: Option[XCollection[A]]
  def isEmpty: Boolean
  def add[B >: A](x: B): XCollection[B]

  def printElements: String

  // polymorphic call
  override def toString = s"[${printElements}]"

  def map[B](f: A => B): XCollection[B]
  def flatMap[B](f: A => XCollection[B]): XCollection[B]
  def filter(p: A => Boolean): XCollection[A]
  def foreach(f: A => Unit): Unit
  def sort(f: (A, A) => Int): XCollection[A]
  def reverse: XCollection[A]
  def zipWith[B, C](l: XCollection[B], f: (A, B) => C): XCollection[C]
  def fold[B](default: B)(f: (B, A) => B): B
  def <>[B >: A](l: XCollection[B]): XCollection[B]

}

case class Empty() extends XCollection[Nothing] {
  def head = None
  def tail= None
  def isEmpty = true
  def add[B >: Nothing](x: B) = Cons(x, Empty())
  def append[B >: Nothing](x: B) = Cons(x, Empty())

  def printElements = ""

  def map[B](f: Nothing => B) = Empty()
  def flatMap[B](f: Nothing => XCollection[B]) = Empty()
  def filter(p: Nothing => Boolean) = Empty()
  def foreach(f: Nothing => Unit) = Unit
  def reverse() = Empty()
  def zipWith[B, C](l: XCollection[B], f: (Nothing, B) => C) = {
    if (l.isEmpty) throw new RuntimeException("List do not have the same length")
    else Empty()
  }
  def fold[B](default: B)(f: (B, Nothing) => B): B = default
  def sort(f: (Nothing, Nothing) => Int) = Empty()

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

  def reverse: XCollection[A] = {
    @tailrec
    def reverseUtil(originalList: XCollection[A] = this, result: XCollection[A] = Empty()): XCollection[A] = {
      if (originalList.isEmpty) result
      else reverseUtil(originalList.tail.get, Cons(originalList.head.get, result))
    }

    reverseUtil()
  }

  def flatMap[B](tr: A => XCollection[B]): XCollection[B] =
    tr(h) <> t.flatMap(tr)

  def filter(p: A => Boolean): XCollection[A] =
      if (p(h)) Cons(h, t.filter(p))
      else t.filter(p)

  def map[B](f: A => B): XCollection[B] =
    Cons(f(h), t.map(f))

  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(f: (A, A) => Int) = {
    @tailrec
    def swap(x: A, sorted: XCollection[A], toSwapAcc: XCollection[A] = Empty()): XCollection[A] =
      if (sorted.isEmpty || f(x, sorted.head.get) < 0) toSwapAcc.reverse <> sorted.add(x)
        else if (f(x, sorted.head.get) == 0) swap(x, sorted.tail.get, toSwapAcc.reverse.add(sorted.head.get))
      else swap(x, sorted.tail.get, toSwapAcc.add(sorted.head.get))

    @tailrec
    def insert(curr: XCollection[A] = this, sorted: XCollection[A] = Empty()): XCollection[A] =
      if (curr.isEmpty) sorted
      else insert(curr.tail.get, swap(curr.head.get, sorted))

    insert()
  }

  def zipWith[B, C](l: XCollection[B], f: (A, B) => C): XCollection[C] = {
    if (l.isEmpty) throw new RuntimeException("XCollections do not have the same length")
    else {
      @tailrec
      def zip(l1: XCollection[A], l2: XCollection[B], acc: XCollection[C] = Empty()): XCollection[C] = {
        if (l1.isEmpty || l2.isEmpty) acc.reverse
        else zip(l1.tail.get, l2.tail.get, acc.add(f(l1.head.get, l2.head.get)))
      }

      zip(this, l)
    }
  }

  def fold[B](acc: B)(f: (B, A) => B): B = {
    def go(curr: XCollection[A] = this, acc: B = acc): B =
      if (curr.isEmpty) acc
      else go(curr.tail.get, f(acc, curr.head.get))

    go()

//    t.fold(f(acc, h))(f)
  }

  def <>[B >: A](l: XCollection[B]): XCollection[B] =
    Cons(h, t <> l)
}

object Test extends App {
  val myList = Cons(1, Cons(4, Cons(5, Empty())))
  val myList2 =  Cons("Hello", Cons("Worldo", Empty()))
  // 1 2 3

  myList foreach {
    x => println(x)
  }

  //  val mapped = myList.map(new Function1[Int, Int] {
    ////    override def apply(a: Int): Int = a * 2
    ////  })

//  def compose(f, g)

  def toCurry[T](f: (T, T) => T): T => T => T = x => y => f(x, y)
  def fromCurry[T](f: T => T => T): (T, T) => T = (x, y) => f(x)(y)
  def andThen[A, B, C](f: A => B, g: B => C): A => C = x => g(f(x))
  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  val mapped = myList.map(_ * 2)
  val filtered = myList.filter(_ % 2 == 0)

  filtered.flatMap(a => Cons(a, Cons(a + 1, Empty())))


  (myList head) map(x => x + 1)

  println(myList.sort((x, y) => y - x))
  println(myList2.zipWith[Int, String](myList, (str, int) => str + "-" + int))
  println(myList.fold(0)(_ + _))
}

