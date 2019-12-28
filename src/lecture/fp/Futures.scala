package lecture.fp

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}

object Futures extends App {

  def heavyCalc: Int = 42

  val aFuture = Future {
    heavyCalc
  }

  // Future {} (global) // implicitly passed to Futures

  aFuture onComplete {
    case Success(value) => println(s"got $value")
    case Failure(_) => println("Failed")
  } // called by some thread

  case class Profile(id: String, name: String) {
    def poke(profile: Profile) = {
      println(s"${this.name} pokes ${profile.name}")
    }
  }

  object SocNetwork {
    val names = Map(
      "1" -> "Zuck",
      "2" -> "Bill",
      "3" -> "Summy"
    )

    val friends = Map(
      "1" -> "2",
      "3" -> "1"
    )

    val random = new Random()

    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val id = friends(profile.id)
      Profile(id, names(id))
    }
  }

  for {
    markProfile <- SocNetwork.fetchProfile("1")
      .recover { case e: Throwable => Profile("X", "dummy")}
      .fallbackTo(SocNetwork.fetchProfile("2"))
    billProfile <- SocNetwork.fetchFriend(markProfile).recoverWith {
      case e: Throwable => SocNetwork.fetchFriend(markProfile)
    }
  } yield markProfile.poke(billProfile)

  // fallbacks

  object Bank {
    case class User(name: String)
    case class Transaction(sender: String, receiver: String, amount: Double, status: String)

    def fetchUser(name: String) = Future(User(name))
    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = {
      Future(Transaction(user.name, merchantName, amount, "SUCCESS"))
    }
    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      val futureTransStatus = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      // if the provided duration will pass, an error will be thrown
      Await.result(futureTransStatus, 2.seconds)
    }
  }
}

object Promises {
 def basics(): Unit = {
   // promise -> a Future controller
   val somePromise = Promise[Int]
   val someFuture = somePromise.future

   // consumer
   someFuture onComplete {
     case Success(r) => println(s"[Consumer] got $r")
   }

   // producer
   new Thread {
     println("[Producer] producing...")

     // fulfilling the promise
     somePromise.success(42)
   }
 }

  def fulfill[T](value: T): Future[T] = Future(value)

  def sequence[A, B](first: Future[A], second: Future[B]) =
    first.flatMap(_ => second)

  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]

    // will resolve the promise on the first future completion
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future
  }

  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    val check = (res: Try[A]) => if (!bothPromise.tryComplete(res)) lastPromise.complete(res)

    fa.onComplete(check)
    fb.onComplete(check)

    lastPromise.future
  }

  def retryUntil[A](action: () => Future[A], cond: A => Boolean): Future[A] =
    action()
      .filter(cond)
      .recoverWith {
        case _ => retryUntil(action, cond)
      }

}

object Parallel {
  val parList = List(1, 2, 3).par
  val aParVector = ParVector[Int]()

  var sum = 0
  parList.foreach(sum += _)
  parList.tasksupport


  // map+reduce model
  // map -> split elements into chunks that could be processed by one thread - Splitter
  // reduce -> combine results from different threads in Combiner
}