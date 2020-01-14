package lecture.typesystem

object variance {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  class Cage[T]

  // Should Cage[Cat] inherit from Cage[Animal] ?
  // declaring inheritance type

  // yes - covariance
  class CCage[+T]
  val cage: CCage[Animal] = new CCage[Cat]

  // no - invariance
  class ICage[T]
  // err: val icage: ICage[Animal] = new ICage[Cat]

  // no - the opposite - contravariance
  class XCage[-T]
  val xcage: XCage[Cat] = new XCage[Animal]

  class InvariantCage[T](animal: T) // invariant

  // covariant positions
  class CovariantCage[+T](val animal: T)

  // err: contravaraint type in covariant position
  // class ContravariantCage[-T](val animal: T)
  // val catCage: XCage[Cat] = new XCage[Animal](new Dog)

  // err: covariant type in contravariant position
  // class CovariantCage[+T](var animal: T)
  //  val ccage: CCage[Animal] = new CCage[Cat](new Cat)
  //  ccage.animal = new Dog

  class AnotherCovariantCage[+T] {

    // covariant type in contravariant position
   // def addAnimal(animal: T): Unit
  }

  // collection Animal := collection Dog
  // collection Animal .add Cat ??

  class AnotherContravariantCage[-T] {
    def addAnimal(animal: T) = true
  }

  // collection Cat := collection Animal
  // collection Cat .add Cat
  // err: collection Cad .add Animal

  class PetShop[-T] {
    def get[S <: T](isIt: Boolean, default: S): S = default
  }

  // type widening
  class XList[+A] {
    // B is a supertype of A: [Animal >: Doggo]
    def add[B >: A](elem: B): XList[B] = new XList[B] // will return more common type
  }

  // method arguments are in contravariant positions
  // return types are in covariant positions

  trait Func[-A, +B] {
    def apply(a: A): B
  }


  ///
  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  class IList[T]

   // invariant
  // container of only one type
  class IParking[T](vehicles: List[T]) {
   def park(vehicle: T): IParking[T] = ???
   def impound(vehicles: List[T]):  IParking[T] = ???
   def check(cond: String): List[T] = ???
   def flatMap[S](a: T => IParking[S]): IParking[S] = ???
 }

  // covariant - good for collections
  class CoParking[+T](vehicles: List[T]) {
    def park[B >: T](vehicle: B): CoParking[B] = ???
    def impound[B >: T](vehicles: List[B]):  CoParking[B] = ???
    def check(cond: String): List[T] = ???
    def flatMap[S](a: T => CoParking[S]): IParking[S] = ???
  }

  // contravariant - good for group of actions
  class ContrParking[-T](vehicles: List[T]) {
    def park(vehicle: T): ContrParking[T] = ???
    def impound(vehicles: List[T]):  ContrParking[T] = ???
    def check[B <: T](cond: String): List[B] = ???
    // A is double contravariant (F[-A, +S]) so it becomes
    // covariant and makes `a` arg covariant which gives error
    // type narrowing is required
    def flatMap[A <: T, S](a: A => ContrParking[S]): ContrParking[S] = ???
  }

  // covariant - good for collections
  class CoParking2[+T](vehicles: IList[T]) {
    def park[B >: T](vehicle: B): CoParking2[B] = ???
    def impound[B >: T](vehicles: IList[B]):  CoParking2[B] = ???
    def check[B >: T](cond: String): IList[B] = ???
  }

  // contravariant - good for group of actions
  class ContrParking2[-T](vehicles: IList[T]) {
    def park(vehicle: T): ContrParking2[T] = ???
    def impound[B <: T](vehicles: IList[B]):  ContrParking2[B] = ???
    def check[B <: T](cond: String): IList[B] = ???
  }


}
