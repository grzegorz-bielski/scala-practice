package lecture.fp

import scala.annotation.tailrec

object TuplMaps extends App {

  // tuples
  val aTuple0 = Tuple2(2, "hello")
  val aTuple = (2, "hello")

  val newTpl = aTuple.copy(_2 = "lo")
  aTuple.swap

  // maps

  val map = Map(("Hi", 23), ("aj", 1), ("AJ", 2) )
  val map2 = Map("Hi" -> 23, "aj" -> 1).withDefaultValue(0)

  println(map2)

  map.contains("Hi")
  map("Hi")
  val newMap = map + ("Sth" -> 45)

  // map, flatMap, filter
  newMap.map(kv => kv._1.toLowerCase -> kv._2)

  newMap.filterKeys(_.startsWith("H"))
  newMap.mapValues(_ + 1)

  // conv

  newMap.toList

  type Net[T] = Map[T, Set[T]]


  def add[T](net: Net[T], person: T): Net[T] = {
    net + (person -> Set())
  }

  def unfriend[T](net: Net[T], a: T, b: T): Net[T] = {
    net + (a -> (net(a) - b)) + (b -> (net(b) - a))
  }

  def friend[T](net: Net[T], a: T, b: T): Net[T] = {
    net + (a -> (net(a) + b)) + (b -> (net(b) + a))
  }

  def remove[T](net: Net[T], a: T): Net[T] = {
    @tailrec
    def rm(fr: Set[T], acc: Net[T]): Net[T] = {
      if (fr.isEmpty) acc
      else rm(fr.tail, unfriend(acc, a, fr.head))
    }

    rm(net(a), net) - a
  }

  def nFriends[T](net: Net[T], a: T): Int = {
    if (!net.contains(a)) 0 else net(a).size
  }

  def mostFriends[T](net:Net[T]): T = {
    net.maxBy(kv => kv._2.size)._1
  }

  def nNoFriends[T](net: Net[T]): Int = {
//    net.filterKeys(k => net(k).isEmpty).size
    net.count(_._2.isEmpty)
  }

  def hasConnection[T](net: Net[T], a: T, b: T): Boolean = {
    @tailrec
    def check(target: T = b, considered: Set[T] = Set(), discovered: Set[T] = net(a) + a): Boolean = {
      if (discovered.isEmpty) false
      else {
        val p = discovered.head
        if (p == target) true
        else if (considered.contains(p)) check(target, considered, discovered.tail)
        else check(target, considered + p, discovered.tail ++ net(p))
      }
    }

    check()
  }

}

object Optionals {
  None orElse Some("kek")
}

