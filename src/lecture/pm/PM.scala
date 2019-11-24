package lecture.pm

import scala.util.Random

object PM extends App {

  val x = new Random

  x.nextInt(10) match {
    case 1 => "1"
    case _ => "kek"
  }

  // will warn about non exhaustive pattern matching
  sealed class Human
  case class Person(name: String, age: Int) extends Human

  val res = Person("kek", 10) match {
    case Person(m, a) if a < 21 => ""
    case _ => ""
  }

  // nested

  (1, (2, 3)) match {
    case (_, (2, v)) => "" + v.toString
  }

  // list patterns
  List(1, 2, 3, 42) match {
    case List(1, _, _, _) => "f"
    case List(1, _*)  => ""// arbitrary length
    case 1 :: List(_) => ""
    case List(1, 2, 3) :+ 42 => ""
  }

  // last chart decides associativity
  // ends with ":" -> right associative
  val prependedList = 2 :: List(2, 4)


  // type specifiers
  val unknown: Any = 2
  val unknownMatch = unknown match {
    case list: List[Int] => ""
    case _ =>
  }

  // name binding
  val nameBindingMatch = List(2) match {
    case nonEmptyList @ List(_, _) => ""
  }

  val xxy: Option[Int] = Some(1)

  val multipattern = xxy match {
    case Some(1) | Some(3) => ""
    case None => ""
  }

  val head :: tail = List(1, 2, 3)

  val ll =  List(1)
  // partial function literal
  val mapped = ll map {
    case v if v % 2 == 2 => v + " even"
    case 1 => "one"
    case _ => "sth"
  }

  //

  val nums = List(1)
  val desc = nums match {
    case head :: Nil => println("sfwfw")
    case _ => ""
  }

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  object biggerThan70 {
    def unapply(arg: Int): Option[Boolean] =
      if (arg > 70) Some(true) else None
  }

  val n = 45
  val matched = n match {
    case singleDigit() => "single digit"
    case even() => "even"
    case biggerThan70(n) => s"$n is bigger than 70"
    case _ => "no prop"
  }

  case class Or[A, B](a: A, b: B)
  val either = Or(2, "what")
  val sth = either match {
    case number Or string => s"$number $string"
  }

}
