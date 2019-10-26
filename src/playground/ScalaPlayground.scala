package playground

object ScalaPlayground extends App {
  println(Person.N_EYES)

  // scala does not have class-level functionality - statics



  object Person {
    val N_EYES = 2

    def apply(m: Person, f: Person) = new Person("fef")
  }

  class Person(val name: String)

  val someone = Person(new Person("fe"), new Person("feff"))


  // Scala applications - scala object with
  //  def main(args: Array[String]): Unit
}

object Sth {}
object SthElse {}