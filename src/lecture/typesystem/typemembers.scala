package lecture.typesystem

object typemembers extends App {
  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class XColl {
    type SomeType
    type BoundedAnimal <: Animal
    type Alias = Cat
  }

  val xc = new XColl
  val dog: xc.SomeType = ???
  val dog2: xc.Alias = new Cat

  trait XList {
    type T
    def add(elem: T): XList
  }

  class XXList(value: Int) extends XList {
    override type T = Int
    def add(elem: Int): XList = ???
  }

  // path dependent types

  class Outer {
    class Inner
    object InnerObject

    def printGeneral(i: Outer#Inner) = print(i)
  }

  val o = new Outer
  val inner = new o.Inner

  val oo = new Outer
  //  val other: oo.Inner = inner

  o.printGeneral(inner)
  oo.printGeneral(inner)

  trait ItemLike {
    type Key
  }

  trait Item[K] extends ItemLike {
    type Key = K
  }
  trait IntItem extends Item[Int]
  trait StringItem extends Item[String]


  def get[ItemType <: ItemLike](key: ItemType#Key): ItemType = ???

  get[IntItem](42)
//  get[IntItem]("fe")

}
