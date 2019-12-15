package lecture.fp

import scala.annotation.tailrec

abstract class XStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: XStream[A]
  def #::[B >: A](elem: B): XStream[B] // prepend
  def ++[B >: A](stream: => XStream[B]): XStream[B]
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): XStream[B]
  def flatMap[B](f: A => XStream[B]): XStream[B]
  def filter(p: A => Boolean): XStream[A]
  def take(n: Int): XStream[A]
  def takeAsList(n: Int): List[A]

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

class XEmpty extends XStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail: XStream[Nothing]= throw new NoSuchElementException
  def #::[B >: Nothing](elem: B): XStream[B] = new XCons(elem, this)
  def ++[B >: Nothing](stream: => XStream[B]): XStream[B] = stream
  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): XStream[B] = this
  def flatMap[B](f: Nothing => XStream[B]): XStream[B] = this
  def filter(p: Nothing => Boolean): XStream[Nothing] = this
  def take(n: Int): XStream[Nothing] = this
  def takeAsList(n: Int): List[Nothing] = Nil
}

class XCons[+A](h: A, t: => XStream[A]) extends XStream[A] {
  def isEmpty: Boolean = false
  override val head: A = h
  override lazy val tail: XStream[A] = t
  def #::[B >: A](elem: B): XStream[B] = new XCons(elem, this)
  def ++[B >: A](stream: => XStream[B]): XStream[B] =
    // still lazy because t is call by name
    new XCons[B](head, tail ++ stream)
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  def map[B](f: A => B): XStream[B] = new XCons[B](f(head), tail.map(f))
  def flatMap[B](f: A => XStream[B]): XStream[B] = f(head) ++ tail.flatMap(f)
  def filter(p: A => Boolean): XStream[A] =
    if (p(head)) head #:: tail.filter(p)
    else tail.filter(p) // preserve lazy val

  def take(n: Int): XStream[A] =
    if (n == 0) new XEmpty
    else head #:: tail.take(n - 1)


  def takeAsList(n: Int): List[A] = take(n).toList()
}

object XStream {
  def from[A](start: A)(gen: A => A): XStream[A] =
    new XCons(start, XStream.from(gen(start))(gen))
}

object PlaygroundX extends App {
   val naturals = XStream.from(1)(x => x + 1)
   val finiteNaturals = naturals.take(100)

    val startFrom0 = 0 #:: naturals

//    naturals.take(100).foreach(println)

    println(startFrom0.head)
    println(naturals.tail.head)

    println(
      startFrom0
        .map(_ * 2)
        .flatMap(x => new XCons(x, new XCons(x + 1, new XEmpty())))
        .take(10)
        .toList()
    )

    def fib(first: BigInt, second: BigInt): XStream[BigInt] =
      new XCons(first, fib(first, first + second))

    def eratosthenes(nums: XStream[Int]): XStream[Int] =
      if (nums.isEmpty) nums
      else new XCons(
        nums.head,
        eratosthenes(
          nums.tail.filter(_ % nums.head != 0 )
        )
      )
}



