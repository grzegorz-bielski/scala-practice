package lecture.typesystem

object fboundedpolymorphism extends App {
  // how to force a method in super type to accept sub type?

    // solution 1
    // recursive type: F-Bounded Polymorhpism
    // + self type bound for not using T different than class type in Animal[T]
    // limited: it's stop being effective for the class that's extending bounded class
   trait AnimalB[A <: AnimalB[A]] { this: A =>
     def breedB: List[AnimalB[A]]
   }

   class Cat extends AnimalB[Cat] {
    override def breedB: List[AnimalB[Cat]] = ???
   }

  // solution 2
  // typeclasses

  trait Animal
  trait Breedable[A] {
    def breed(a: A): List[A]
  }

  class Dog extends Animal
  object Dog {
    implicit object DogsCanBreed extends Breedable[Dog] {
      def breed(a: Dog): List[Dog] = List()
    }
  }

  implicit class BreedableEnrichment[A <: Animal](animal: A) {
    def breed(implicit breedable: Breedable[A]): List[A] = breedable.breed(animal)
  }

  val dog = new Dog
  dog.breed

  // solution 3
  // pure typeclass

  trait AnimalD[A] {
    def breed(a: A): List[A]
  }

  class DogD
  object DogD {
    implicit object DogDAnimal extends AnimalD[DogD] {
      override def breed(a: DogD): List[DogD] = List()
    }
  }

  implicit class AnimalDOps[A](animal: A) {
    def breed(implicit instance: AnimalD[A]): List[A] = instance.breed(animal)
  }

}
