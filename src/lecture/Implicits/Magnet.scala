package lecture.Implicits

import scala.concurrent.Future

object Magnet extends App {

  class P2PRequest
  class P2PResponse
  class Serializer[T]

  trait Actor {
      // def receive[T](msg: T)(implicit serializer: Serializer[T]): Int
    def receive[T : Serializer](msg: T): Int
    def receive(future: Future[P2PRequest]): Int
    def receive(statusCode: Int): Int
  }

  // normal overloading:
  // 1. type erasure
  // 2. lifting (for partial application) doesn't work with all overloads
  // 3. code duplication
  // 4. type inference and default args problem

  trait MessageMagnet[Result] {
    def apply(): Result
  }

  // will choose right implicit class
  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(req: P2PRequest) extends MessageMagnet[Int] {
    def apply(): Int = {
      println("FromP2PRequest")
      42
    }
  }

  implicit class FromP2PResponse(req: P2PResponse) extends MessageMagnet[Int] {
    def apply(): Int = {
      println("FromP2PResponse")
      24
    }
  }

  receive(new P2PResponse)
  receive(new P2PRequest)

  trait AddMagnet {
    def apply(): Int
  }

  def add1(magnet: AddMagnet): Int = magnet()

  implicit class AddInt(x: Int) extends AddMagnet {
    def apply(): Int = x + 1
  }
  implicit class AddString(s: String) extends AddMagnet {
    def apply(): Int = s.toInt + 1
  }

  val addFunc = add1 _

  addFunc(1)
  addFunc("3")

  // magnet
  // 1. no more type erasure
  // 2. func lifting works
  // 3. verbose... and harder to read
  // 4. can't name and use default args
  // 5. no call by name

}
