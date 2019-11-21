package lecture.fp

import scala.util.Random

object Sequences extends App {

  // seq
  val aSeq = Seq(1, 2, 3)
  aSeq.reverse
  aSeq(1)
  aSeq ++ Seq(5, 6 ,7)
  aSeq.sorted

  // range
  val aRange: Seq[Int] = 1 to 10
  (1 until 10).foreach(x => println("heh"))

  // list
  val aList = List(1, 2, 3)
  val prepend = 42 :: aList
  val prepend2 = 42 +: aList
  val append = aList :+ 42

  val apple5 = List.fill(5)("Apple")

  println(aList.mkString("-"))

  // arrays
  val numArr = Array(1, 2, 3)
  val treeElemArr = Array.ofDim[Int](3)
  // default values of the provided types => 0

  // syntax sugar for .update(2, 0)
  treeElemArr(2) = 0

  // implicit conversion
  val converted: Seq[Int] = treeElemArr

  // Vectors
  // default immutable seq impl
  val vec = Vector(1, 2, 4)

  // more performent than list for large collections
  val maxRuns = 1000
  val maxCapacity = 10000

  // + keeps refs to tail
  // - takes a lot of time to update elem in the middle
  val testNumList = (1 to maxCapacity).toList

  // + depth of tree is small
  // - need to replace an 32-elemnt chunk
  val testNumVec = (1 to maxCapacity).toVector

  println(getWriteTime(testNumList))
  println(getWriteTime(testNumVec))

  def getWriteTime(coll: Seq[Int]): Double = {
    val r = new Random

    val times = for {
      it <- 1 to 1000
    } yield {
      val curr = System.nanoTime()
      coll.updated(r.nextInt(maxCapacity), 0)
      System.nanoTime() - curr
    }

    times.sum * 1.0 / maxRuns
  }

}

// ordered, indexed
trait SeqPrim[+A] {
  def head: A
  def tail: Seq[A]
}

