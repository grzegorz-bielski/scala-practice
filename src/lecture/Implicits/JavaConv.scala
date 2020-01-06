package lecture.Implicits

import java.{util, util => ju}

object ScalaJavaConv extends App {
  import collection.JavaConverters._

  val javaSet: ju.Set[Int] = new ju.HashSet[Int]()
  val scalaSet = javaSet.asScala // implicit def conversion from the `collection.JavaConverters`

  import collection.mutable._
  val numbersBuffer = ArrayBuffer[Int](1, 2, 3)
  val juNumbersBuffer = numbersBuffer.asJava

}
