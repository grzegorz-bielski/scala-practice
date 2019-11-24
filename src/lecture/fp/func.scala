package lecture.fp

import scala.annotation.tailrec

object func extends App {

  val doubler = new MyFunc[Int, Int] {
    override def apply(a: Int): Int = a * 2
  }

  val doubled = doubler(2)

  // function types

  val strToIntConv = new Function[String, Int] {
    override def apply(v1: String): Int = v1.toInt
  }

  val adder: ((Int, Int) => Int) = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  val concatStr = new ((String, String) => String) {
    override def apply(v1: String, v2: String): String = v1.concat(v2)
  }

  val adderPrim = new Function1[Int, Function1[Int, Int]] {
    override def apply(x: Int): Function1[Int, Int] = new Function[Int, Int] {
      override def apply(y: Int): Int = x + y
    }
  }

  val adderPrim2 = (a: Int) => (b: Int) => a + b

  val y = adderPrim(1)(2)

  val someInt = strToIntConv("1")


  // syntactic sugar

  val adder2 = (x: Int) => x + 2

  val doSth: () => Int = () => 3

  val stillAfunc = doSth

  val stringToIn = { (str: String) =>
    str.toInt
  }

  val incr: Int => Int = _ + 1
  val incr2: (Int, Int) => Int = _ + _


  @tailrec
  def nTimes[T](f: T => T, n: Int, x: T): T =
    if (n <= 0) x
    else nTimes(f, n - 1, f(x))

  def nTimesPrim[T](f: T => T, n: Int): T => T =
    if (n <= 0) (x: T) => x
    else (x: T) => nTimesPrim(f, n - 1)(f(x))

  val plus20 = nTimesPrim(adder2, 10)
  val xyz = plus20(1)

}

object compreh extends App {
  // map, flatMap, for comprehensions

  val nums = List(1, 2, 3, 4)
  val strs = List('a', 'b', 'c', 'd')
  val things = List("xx", "yy")
  nums.head
  nums.tail

  nums.map { x => x * 2}

  nums.flatMap(x => List(x, x))
  println(nums.flatMap(x => strs.flatMap(y => things.map(t => s"$x$y$t"))))

  val res = for {
    x <- nums if x % 2 == 0
    y <- strs
    t <- things
  } yield s"$x$y$t"


  for {
    n <- nums
  } println(n)

  def singleArgMethod(arg: Int): String = s"$arg sth"

  val desc = singleArgMethod {
    val x = 40
    x + 42
  }

  // compiler can convert single methods traits / abstract classes to lambdas
  trait Action {
    def act(x: Int): Int
  }

  val funcInstance: Action = (a: Int) => a

  class Kek {
    def `some method` = "hey"
  }

  class Composite[A, B]
  val composite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???


}

trait MyFunc[A, B] {
  def apply(a: A): B
}