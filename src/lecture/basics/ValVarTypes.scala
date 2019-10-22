package lecture.basics

import scala.annotation.tailrec

object ValVarTypes extends App {

  val aString: String = "heh"
  val aBoolean: Boolean = false
  val aChar: Char = 'f'
  val anInt: Int = 3
  val aShort: Short = 3456
  val aLong = 23423423L
  val aLong2: Long = 23423423
  val aFloat = 324.33f
  val xx: Double = 3.14 // default


  def greet(name: String, age: Int) =
      s"Hi my name is $name and I am $age years old."

  def factorial(n: Int): Int =
    if ( n == 1 ) n else n * factorial(n-1)

  def factorial2(n: Int): Int = {
    @tailrec // check if tail recursion is applied
    def go(x: Int, acc: Int): Int =
      if (x == 1) acc
      else go(x - 1, x * acc) // tail recursion (function call is last expression)
    go(n, 1)
  }

  def fib(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }

  def fibRec(n: Int): Int = {
    def go(i: Int, lasts: (Int, Int) = (1, 1)): Int =
      if (i >= n) lasts._1
      else go(i + 1, (lasts._1 + lasts._2, lasts._1))

    if (n <= 2) 1 else go(n)
  }

  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Double): Boolean =
      if (t <= 1) true
      else (n % t != 0) && isPrimeUntil(t - 1)

    val boundary = Math.ceil(Math.sqrt(n))

    isPrimeUntil(boundary)
  }

  def isPrimeTailRec(n: Int): Boolean = {
    def isPrimeUntil(t: Double, isStillPrime: Boolean = true): Boolean =
      if (!isStillPrime) false
      else if (t <= 1) true
      else isPrimeUntil(t - 1, n % t != 0 && isStillPrime)

    val boundary = Math.ceil(Math.sqrt(n))

    isPrimeUntil(boundary)
  }


  def con(n: Int, str: String) = {
    def go(n: Int, acc: String = ""): String = {
      if (n <= 0) acc
      else go(n - 1, acc + str)
    }
    go(n)
  }

  // value is computed before call
  def callByValue(x: Long): Unit = {
    println("by value: " + x)
    println("by value: " + x)
  }

  // value is computed for each use
  // x will be replaced by System.nanoTime() call
  def callByName(x: => Long): Unit = {
    println("by name: " + x)
    println("by name: " + x)
  }

  def infinite(): Int = 1 + infinite()
  def printFirst(x: Int, y: => Int) = println(x)

  // the call to the infinite is lazy
  printFirst(42, infinite())

//  callByValue(System.nanoTime())
//  callByName(System.nanoTime())

//  println(con(7, "jok"))

//  def isPrime(n: Int)
//  println(greet("kek", 1))
//  println(factorial(3))
}

object StrOps extends App {
  val str = "3rwefrw"
  str.charAt(2)
  str.substring(7, 11)
  str.split("").toList
  str.replace(" ", "-")
  str.toLowerCase()
  str.length

  str.toInt

  val prepended = 'a' +: str
  val appended = str :+ 'b'
  str.reverse
  str.take(2)

  // s interpolation
  val sthh = s"sth $prepended heh ${1 + 2}"

  // f interpolation
  val speed = 1.2f
  val sthhh = f"$prepended can sthh $speed%2.2f kek"

  // raw interpolation
  val sth = raw"This is not interpreted \n ty"
}
