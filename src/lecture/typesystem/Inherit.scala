package lecture.typesystem

object Inherit extends App {
  trait Writer[T] {
    def write(value: T): Unit
  }

  trait Closable {
    def close(status: Int): Unit
  }

  trait GenericStream[T] {
    def foreach(f: T => Unit): Unit
  }

  def processStream[T](stream: GenericStream[T] with Writer[T] with Closable): Unit = {
    stream.foreach(println)
    stream.close(0)
  }

  // diamond problem
  trait Animal { def name: String }
  trait Lion extends Animal {
    override def name: String = "lion"}
  trait Tiger extends Animal {
    override def name: String = "tiger"
  }
  // name: tiger, last override gets picked
  trait Other extends Lion with Tiger

  // super + type linearization problem
  trait Cold {
    def print: Unit = {
      println("cold")
    }
  }

  trait Green extends Cold {
    override def print: Unit = {
      println("green")
      super.print
    }
  }

  trait Blue extends Cold {
    override def print: Unit = {
      println("blue")
      super.print
    }
  }

  class Red {
    def print: Unit = println("red")
  }

  // White = AnyRef with Red{} with Cold{} with Green{} with Blue{} with White{}
  // (while expanding we are crossing)

  // super will call the first on the left from the last object in inheritance hierarchy
  // White -> (super) -> Blue -> ...

  // white, blue, green, cold
  class White extends Red with Green with Blue {
    override def print: Unit = {
      println("white")
      super.print
    }
  }

}
