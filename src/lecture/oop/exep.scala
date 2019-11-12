package lecture.oop

object exep extends App {
  //  val weirdVal = throw new NullPointerException

  // java.lang.OutOfMemoryError
  // val arr = Array.ofDim(Int.MaxValue)

  // java.lang.StackOverflowError
  //  def infinite: Int = 1 + infinite
  //  infinite

  // throwable classes
  // Error and Exception are mayor  Throwable subtypes
  // Exception - sth wrong with the program, Error - sth wrong with the system

  def getInt(withExceptions: Boolean): Int =
    if (withExceptions) throw new RuntimeException("fe")
    else 42

  val x: Int = try {
    getInt(true)
  } catch {
    case e: RuntimeException => {
      printf("caught a Runtime exception")
      3
    }
  } finally {
    // does not influence the return type of expression
    println("finally")
  }
}

class OverflowException extends RuntimeException
class UnderflowException extends RuntimeException
class MathCalcException extends RuntimeException

object Calc {
  def add(x: Int, y: Int) = {
    val res = x + y

    if (x > 0 && y > 0 && res < 0) {
      throw new OverflowException
    }

    if (x < 0 && y < 0 && res > 0) {
      throw new UnderflowException
    }

    res
  }

  def sub(x: Int, y: Int) = {
    val res = x - y

    if (x > 0 && y < 0 && res < 0) {
      throw new OverflowException
    }

    if (x < 0 && y > 0 && res > 0) {
      throw new UnderflowException
    }

    res
  }

  def div(x: Int, y: Int) = {
    if (y == 0) {
      throw new MathCalcException
    }

    ///

    x / y
  }
}