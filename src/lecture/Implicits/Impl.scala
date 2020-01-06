package lecture.Implicits

object Impl extends App {
  def basics() = {
    val pair = "Kek" -> "Oh"

    case class Person(name: String) {
      def greet = s"Hi, $name"
    }

    implicit def fromStringToPerson(str: String): Person = Person(str)

    println("Person".greet) // rewritten to fromStringToPerson().greet

    def increment(x: Int)(implicit amount: Int) = x + amount
    implicit val defaultAmount: Int = 10

    val x = increment(2)
  }

  def organizingImplicits() = {
    implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
    val sorted = List(1, 2, 4).sorted

    // implicits:
    // - val/var
    // - objects/classes
    // - accessor methods (defs without parentheses)

    case class Person(name: String, age: Int)

    object AlphabeticNameOrdering {
      implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
    }

    import AlphabeticNameOrdering._

    val ppl = List(
      Person("Steve", 11),
      Person("who", 22),
    )

    ppl.sorted

    // implicit scope
    // 1. local scope
    // 2. imported scope
    // 3. companion objects of all involved types (used as default, best practice)

  }
}
