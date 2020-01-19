package lecture.typesystem

object selftypes extends App {
  // a way for requiring a type to be mixed in

  trait InstrumentalList {
    def play(): Unit
  }

  // self type
  // forces the `Singer` implementer to also implement `InstrumentalList`
  // `self` variable name is ambiguous
  trait Singer { self: InstrumentalList =>
    def sing(): Unit
  }

  class LeadSinger extends Singer with InstrumentalList {
    override def play(): Unit = ???
    override def sing(): Unit = ???
  }

  val singer = new InstrumentalList with Singer {
    override def play(): Unit = ???
    override def sing(): Unit = ???
  }

  class A
  class B extends A // B is A

  trait T
  trait S { this: T => } // S requires T

  // classical dependency injection
  abstract class Component {}
  class ComponentA extends Component
  class ComponentB extends Component

  class DependentComponent(val component: Component)

  // scala's  cake pattern
  trait ScalaComponent {
      def action(x: Int): String
    }
  trait ScalaDependentComponent { self: ScalaComponent =>
    def otherAction(): String = action(1)
  }

  // layer 1
  trait Picture extends ScalaComponent
  trait Stats extends ScalaComponent

  // layer 2
  trait Profile extends ScalaDependentComponent with Picture
  trait Analytics extends ScalaDependentComponent with Stats

  // cyclical dependency
  trait X { this: Y => }
  trait Y { this: X => }


}
