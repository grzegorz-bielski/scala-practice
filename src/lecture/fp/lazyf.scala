package lecture.fp

object lazyf extends App {
  // lazy vals are evaluated once,
  // but only when they're accessed the first time
  lazy val x: Int = throw new RuntimeException

  def sideEffectCond: Boolean = {
    println("Foo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCond = sideEffectCond

  // lazyCond is never evaluated if the `simpleCondition` is false
  val res = if (simpleCondition && lazyCond) "yes" else "no"

  def byName(n: => Int): Int = n + n + n + 1 // n evaled 4 times
  def byNeed(n: => Int) = {
      lazy val x = n // evaled 1 time
      x + x + x + 1
  }
  // parametress method, evaled to value
  def theAnswer = {
    Thread.sleep(1000)
    42
  }

  println(byNeed(theAnswer))

  val someList = List(1, 25, 40, 5, 23)
  val filtered = someList.filter(x => x > 3)
  val filteredLazy = someList.withFilter(x => x > 3) // not evald until it's needed

  //  using withFilter under the hood
  for {
    a <- someList if a % 2 == 0
  } yield a + 1

  val LazyInstance = Lazy {
    println("side effect")
    42
  }

}

class Lazy[+A](value: => A) {
  // call by need
  private lazy val internal = value
  def use: A = internal
  def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internal)
}

object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy(value)
}