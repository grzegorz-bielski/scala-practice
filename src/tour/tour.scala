package tour

import scala.util.Random
import scala.math._

object Tour extends App {
  //// classes

  class Point(var x: Int, var y: Int) {
    private var _secret = 0
    private val bound = 100

    def secret = _secret
    def secret_= (newVal: Int): Unit = {
      if (newVal < bound) {
        _secret = newVal
      }
    }

    def move(dx: Int, dy: Int): Unit = {
      x = x + dx
      y = y + dy
    }

    override def toString: String = s"($x, $y)"
  }

  val point = new Point(2, 3)

  println(point.x)

  /// traits

  trait Iterator[A] {
    def hasNext: Boolean
    def next(): A
  }

  class IntIterator(to: Int) extends Iterator[Int] {
    private var curr = 0

    def hasNext = curr < to

    def next: Int = {
      if (hasNext) {
        val temp = curr
        curr += 1
        temp
      } else 0
    }
  }

  var iter = new IntIterator(10)
  println(iter.next())
  println(iter.next())

  // tuples

  val ingredient = ("Milk", 2)

  val first = ingredient._1
  val (name, quantity) = ingredient

  val planetes = List(
    ("Mercury", 57.9),
    ("Venus", 108.2),
    ("Earth", 149.6),
    ("Mars", 227.9),
    ("Jupiter", 778.3)
  )

  val sth = planetes.map{
    case ("Earth", distance) => distance + 1
    case _ => 0
  }

  val sthh = for ((a, b) <- planetes) { a }

  println(sth)

  //// mixins composition

  abstract class A {
    val msg: String
  }

  class B extends A {
    val msg = "b"
  }

  // mixin C
  trait C extends A {
    def loudMsg = msg.toUpperCase()
  }

  // classes can only have one superclass but many mixins
  class D extends B with C

  val d = new D
  println(d.msg)
  println(d.loudMsg)

  // typeclass
  abstract class AbsIterator {
    type T
    def hasNext: Boolean
    def next(): T
  }

  // typeclass implementation
  class StringIterator(s: String) extends AbsIterator {
    type T = Char
    private var i = 0
    def hasNext = i < s.length
    def next = {
      val char = s charAt i
      i += 1
      char
    }
  }

  // typeclass with free implementation
  // doesn’t need to implement the abstract members of AbsIterator.
  // used as a mixin
  trait RichIterator extends AbsIterator {
    def foreach(f: T => Unit): Unit = while (hasNext) f(next())
  }

  class RichStrIter(s: String) extends StringIterator(s) with RichIterator
  val richStrIter = new RichStrIter("scala")
  richStrIter foreach println

  //// HOFs

  val salaries = Seq(20000, 7000, 40000)
  val double = (x: Int) => x * 2
  val newSalaries = salaries.map(double)
  // val newSalaries = salaries.map(x => x * 2)
  // val newSalaries = salaries.map(_ * 2)

  case class WeeklyWeatherForecast(temps: Seq[Double]) {
    private def covertCtoF(temp: Double) = temp * 1.8 + 32
    def forecastInFahrenheit: Seq[Double] = temps.map(covertCtoF)
  }
  object SalaryRaiser {
    private def promote(salaries: List[Double], fn: Double => Double): List[Double] =
      salaries.map(fn)

    def smallPromotion(salaries: List[Double]): List[Double] =
      promote(salaries, _ * 1.1)
  }

  def urlBuilder(ssl: Boolean, domain: String): (String, String) => String = {
    val schema = if (ssl) "https://" else "http://"
    (endpoint: String, query: String) => s"$schema$domain/$endpoint?$query"
  }
  val getURL = urlBuilder(ssl=true, "www.example.com")
  val url = getURL("users", "id=1")

  // currying

  val nums = List(1, 2, 4, 5)
  val red = nums.foldLeft(0)((acc, curr) => acc + curr)
  // val red = nums.foldLeft(0)(_ + _)
  def execute(arg: Int)(implicit ec: scala.concurrent.ExecutionContext): Unit = {}
  // partial application
  val emptyList = List[Int]()
  val foldWith = nums.foldLeft(emptyList) _
  val squares = foldWith((xs, x) => xs :+ x*x)

  // case classes
  // compared by structure, not reference

  case class Book(isbn: String, available: Boolean = true)

  val someBook = Book("978-0486282114")
  val someOtherBook = Book("272-0383792110")
  val someOtherBook2 = someOtherBook.copy(available = false)
  val isbn = someBook.isbn
  val areTheSame = someBook == someOtherBook

  // pattern matching

  val x = Random.nextInt(10)

  val xx = x match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "other"
  }

  // singletons and companion objects

  case class Circle(radius: Double) {
    import Circle._
    def area: Double = calculateArea(radius)
  }

  object Circle {
    private def calculateArea(radius: Double): Double = Pi * pow(radius, 2.0)
  }

  val area = Circle(5.0).area

  class Email(val username: String, val domainName: String)

  object Email {
    def fromString(emailStr: String): Option[Email] = {
      emailStr.split('@') match {
        case Array(a, b) => Some(new Email(a, b))
        case _ => None
      }
    }
  }

  Email.fromString("scala.center@epfl.ch") match {
    case Some(email) => println(
      s"""Registered an email
         | Username: ${email.username}
         | Domain: ${email.domainName}
       """)
    case None => println("Error: could not parse email")
  }

  // extractor objects

  object CustomerID {
    // constructor method
    def apply(name: String) = s"$name--${Random.nextLong}"

    // takes object and gives back arguments
    def unapply(customerID: String): Option[String] = {
      val strArr = customerID.split("--")
      if (strArr.tail.nonEmpty) Some(strArr.head) else None
    }
  }

  CustomerID("kek") match {
    // unapply
    case CustomerID(name) => println(name)
    case _ => println("Could not extract")
  }

  // for comprehensions

  // for (enumerators) yield e
  // enumerators refers to a semicolon-separated list of enumerator
  // comprehension evaluates the body e for each binding generated
  // by the enumerators and returns a sequence of these values

  case class User(name: String, age: Int)

  val userBase = List(
    User("Travis", 28),
    User("Kelly", 33),
    User("Jennifer", 44),
    User("Dennis", 23)
  )
  val sthList = for (user <- userBase if (user.age >= 20 && user.age < 30))
    yield user.name

  // nested
  def foo(n: Int, v: Int) =
    for (i <- 0 until n;
         j <- 0 until n if i + j ==v) // nothing is yielded at if this is not returning next value
      yield (i, j) // yield could be omitted for side-effects

  foo(10, 10) foreach {
    case (i, j) => println(s"($i, $j)")
  }


  // generics
  class Stack[A] {
    private var elements: List[A] = Nil
    def push(x: A) { elements = x :: elements }
    def peek: A = elements.head
    def pop(): A = {
      val currentTop = peek
      elements = elements.tail
      currentTop
    }
  }
  class Fruit
  class Apple extends Fruit
  class Banana extends Fruit

  // invariant subtyping
  val stack = new Stack[Fruit]
  stack.push(new Apple)
  stack.push(new Banana)

  // variances

  // Variance is the correlation of subtyping relationships
  // of complex types and the subtyping relationships of
  // their component types.

  class Foo[+A] // covaraint
  class Bar[-A] // contravaraint
  class Baz[A] // invariant


  // covariance (sealed abstract class List[+A])
  abstract class Animal { def name: String }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  object CovarianceTest {
    def printAnimalNames(animals: List[Animal]): Unit = {
      animals.foreach{a => println(a.name)}

    }

    val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
    val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))
    printAnimalNames(cats)
    printAnimalNames(dogs)
  }
  // contravariance
  abstract class Printer[-A] { def print(value: A): Unit }
  class AnimalPrinter extends Printer[Animal] {
    def print(animal: Animal): Unit = println("Animal name is: " + animal.name)
  }
  class CatPrinter extends Printer[Cat] {
    def print(cat: Cat): Unit = println("The cat's name isL " + cat.name)
  }
  object ContravarianceText {
    val cat = Cat("Boots")

    def printCat(printer: Printer[Cat]): Unit = {
      printer.print(cat)
    }

    val catPrinter: Printer[Cat] = new CatPrinter
    val animalPrinter: Printer[Animal] = new AnimalPrinter

    printCat(catPrinter)
    printCat(animalPrinter)
  }
}