package lecture.fp

object func2 extends App {

    // partial functions
    val func: (Int) => String = {
      case 1 => "2"
    }

    val aFunc1 = (x: Int) => x + 1
    val partialFunc: PartialFunction[Int, Int] = {
      case 1 => 42
      case 2 => 56
      case 3 => 999
    }

    // lift to total function x -> Option y
    val maybeLifted = partialFunc.lift

    partialFunc orElse[Int, Int] { case 5 => 1 }
    partialFunc applyOrElse[Int, Int](3, _ => 0)

    List(1, 2, 4) map {
      case 1 => 42
    } orElse[Int, Int] {
      case 2 => 1
    } orElse[Int, Int] {
      case 4 => 1
    }

  val partialFunc2 = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 42
    }
    override def isDefinedAt(x: Int): Boolean = x == 1
  }

  scala.io.Source.stdin
      .getLines
      .map {
        case "hello" => "hi"
        case _ => "what"
      }
      .foreach(println)
}
