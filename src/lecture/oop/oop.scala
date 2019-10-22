package lecture.oop

object oop extends App {
  val p = new Person("ewf", 23)
  p.age
  // p.name not a field

//  println(new Counter(0).inc(2).inc.inc.count)

  class PersonM(val name: String, fav: String, age: Int = 0) {
    def likes(movie: String): Boolean = movie == fav

    def apply(): String = s"Hi $name here"

    def apply(n: Int) = s"Hi $name, $n times"

    // all operators are methods
    def |-(person: PersonM) = "something"

    def +(name: String) = new PersonM(name, fav)

    def learns(str: String) = s"$name learns $str"

    def learnsScala = learns("Scala")

    def unary_+ : PersonM = new PersonM(name, fav, age + 1)

    def unary_! : String = "whatever"

    def isAlive: Boolean = true
  }

  val someone = new PersonM("Doe", "intt")
  someone.likes("erf")
  // infix notation
  someone likes "srr"

  val res = someone |- new PersonM("fw", "ef")

  // prefix unary operators
  // works only for - + ~ !
  val x = -1
  val y = 1.unary_-

  // postfix (methods without params)
  val sth = someone isAlive
  val xx = someone learnsScala

  // apply
  someone.apply()
  someone()
}

class Person(name: String, val age: Int) {
  val x = 2 // field

  println(1 + 4) // eval for every instance

  def greet(name: String): Unit = println(s"${this.name} says: Hi $name")

  // name is implied (this.name)
  def greet(): Unit = println(s"says: Hi $name")

  // overloading constructors
  def this(name: String) = this(name, 0)

}

class Counter(val count: Int) {
  def inc = new Counter(count + 1)
  def dec = new Counter(count - 1)

  def inc(x: Int): Counter = next((_x) => inc.inc(x - 1))(x)
  def dec(x: Int) = new Counter(count - x)

  protected def next(f: (Int) => Counter)(x: Int) =
    if (x <= 0) this else f(x)

}

