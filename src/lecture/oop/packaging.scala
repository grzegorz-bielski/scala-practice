package lecture.oop

import tour.Tour
import playground.{Sth, SthElse => SthPrim}

object packaging extends App {
  val implTest = Tour.ImplicitTest

  val imlTest2 = tour.Tour.ImplicitTest

  val sth = Sth
  val sthPrim = SthPrim

  // package members are accessible by their name
  val person = new Person("fe")

  // packages are hierarchical, matching the folder structure

  // default imports
  //  java.lang, scala, scala.Predef
}
