package lecture.typesystem

object structural extends App {
  type JavaClosable = java.io.Closeable
  class OtherCloseable {
    def close(): Unit = println("close")
  }

  type UnifiedCloseable = {
    def close(): Unit
  }

  // type refinement
  type SilentUnifiedClosable = JavaClosable {
    def silentClose(): Unit
  }

  def closeIt(closable: UnifiedCloseable): Unit = {
    closable.close()
  }

  closeIt(new JavaClosable {
    override def close(): Unit = println("java close")
  })

  closeIt(new OtherCloseable)

  def altClose(closeable: { def close(): Unit }): Unit = {
    closeable.close()
  }

  // duck typing

  // based on reflection
  // performance drawbacks
  type Duck = {
    def quack(): Unit
  }

  class ActualDuck {
    def quack(): Unit = println("quack")
  }

  class NonDuck {
    def quack(): Unit = println("bark")
  }

  val duckOne: Duck = new ActualDuck
  val duckTwo: Duck = new NonDuck

  // compiler will erase the type parameters on
  // generic methods that are using structural types
}
