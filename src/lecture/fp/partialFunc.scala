package lecture.fp

object partialFunc extends App {

  val adder: Int => Int => Int = x => y => x + y

  def adderPrim(x: Int)(y: Int): Int = x + y
  val adderPrim2 = (x: Int, y: Int) => x + y
  def adderPrim3(x: Int, y: Int): Int = x + y

  val add2 = adder(2)

  //  val add2Prim = adderPrim(2) // err
  val add2Prim: Int => Int = adderPrim(2)
  // create function out of method
  // lifting -> ETA-EXPANSION // often done automatically

  // placeholders

  val add4 = adderPrim(4) _ // do ETA-EXPANSION
  val add4Prim = adderPrim(4)(_)
  val add4PrimPrim = adderPrim3(4, _: Int)

  val x = adderPrim2.curried(7) // convert func to curried version

  //

  def formatter(form: String)(value: Double) = form.format(value)
  val res = formatter("%8.6f")(1 : Int)

  //

  def byName(n: => Int) = n + 1
  def byFunc(f: () => Int): Int = f() + 1

  def method: Int = 42
  def parMethod(): Int = 42

  byName(42) // k
  byName(method) // k
  byName(parMethod()) // k
  byName(parMethod) // k, parMethod is called before
  //  byName(parMethod _) nope
  //  byName(() => 42) nope
  byName((() => 42)()) // k

  byFunc(() => 42) // k
  //  byFunc(method) nope, method is eval'd to value instead of func
  byFunc(parMethod) // k
  byFunc(parMethod _) // k

}
