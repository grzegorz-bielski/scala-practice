package lecture.oop

object traits extends App {


  // can't extend `final` class
  // can only extend `sealed` class in this file
  sealed class Animal(name: String) {
    val creatureType = "wild"
    def eat = println("om")

    // can't override final member
    final def eh = println("om")
  }

  class Cat(name: String) extends Animal(name) {
    override def eat = println("ekk")

    def eats(): Unit = {
      super.eat
    }
  }
}

object AbstractDataTypes extends App {
  abstract class Animal {
    val creatureType: String
    def eat: Unit
  }

  class Dog extends Animal {
    val creatureType: String = "Canine"
    def eat: Unit = println("om nom")
  }

  trait Carnivore {
      def eat(animal: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    val creatureType: String = "reptile"
    def eat: Unit = println("om nom")
    def eat(animal: Animal) = println("om nom")
  }

  //  traits vs abstract classes
  // traits don't have constructor parameters
  // you can extend one abstract class and multiple traits
  // traits are - behaviours, abstract class - things

}