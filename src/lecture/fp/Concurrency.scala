package lecture.fp

import java.util.concurrent.Executors

import scala.collection.mutable
import scala.util.Random

object Concurrency extends App {

 def threads() = {
  // JVM threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  // signal to JVM to start a JVM thread (which runs on sc thread)
  aThread.start()

  // block until the aThread finishes running
  aThread.join()

  //

  val threadHello = new Thread(
    () => (1 to 5).foreach(_ => println("hello"))
  )

  val threadBye = new Thread(
    () => (1 to 5).foreach(_ => println("bye"))
  )

  // different runs - different results
  threadHello.start()
  threadBye.start()

  // executors - handles starting and stoping threads
  val pool = Executors.newFixedThreadPool(20)
  pool.execute(() => println("sth in the thread pool"))
  pool.shutdown()

  //
}

 def synchro() = {
    class Account(var sum: Int)

    def unsafe(account: Account, value: Int): Unit = {
      account.sum -= value
    }

    //  1 - synchronized
    def safeOne(account: Account, value: Int): Unit = {
      // no threads can evaluate this at the same time
      // lock the object's monitor
      account.synchronized {
        account.sum -= value

        // release the lock and wait
        // when allowed to proceed lock the monitor again and continue
        //      account.wait()

        // signal one sleeping thread that it may continue
        // account.notify()
        // account.notifyAll() all may continue
      }
    }
    // 2 - @volatile
    class AccountSafe(@volatile var sum: Int)

    def runInParallel = {
      val account = new Account(5000)

      val threadOne = new Thread(() => {
        unsafe(account, 1000)
      })

      val threadTwo = new Thread(() => {
        unsafe(account, 2000)
      })
    }
  }

 def inception(max: Int, counter: Int): Thread = {
   new Thread(() => {
     if (counter < max) {
       val nextThread = inception(max, counter + 1)
       nextThread.start()
       nextThread.join()
     }
   })
 }

  // thread communication

  // producer-consumer
  def producerConsumer() = {
    class Container {
      private var value: Int = 0

      def isEmpty: Boolean = value == 0
      def set(newValue: Int) = value = newValue
      def get = {
        val result = value
        value = 0
        result
      }
    }
      val container = new Container

      val consumer = new Thread(() => {
          container.synchronized {
            // wait till notify
            container.wait()
          }

          container.get
      })

      val producer = new Thread(() => {
        Thread.sleep(2000)
        val value = 42

        container.synchronized {
          container.set(value)
          container.notify()
        }
      })
  }

  def producerConsumer2(): Unit = {
    val buffer = new mutable.Queue[Int]
    val capacity = 3

    val producer = new Producer(2, buffer, capacity)
    val consumer = new Consumer(1, buffer)

    producer.start()
    consumer.start()
  }

  def producerConsumer3(nProducers: Int, nConsumers: Int): Unit = {
    val buffer = new mutable.Queue[Int]
    val capacity = 3

    (1 to nProducers).foreach(i => new Producer(i, buffer, capacity).start())
    (1 to nConsumers).foreach(i => new Consumer(i, buffer).start())
  }

  def caveats() = {
    case class Friend(name: String) {
      def bow(other: Friend) = {
        this.synchronized {
          println(s"$this: bowing to $other")
          other.rise(this)
          println(s"$this: other $other has risen")
        }
      }

      def rise(other: Friend) = {
        this.synchronized {
          println(s"$this: rising to $other")
        }
      }

      var side = "right"
      def switchSide(): Unit = {
        side = if (side == "right") "left" else "right"
      }

      def pass(other: Friend): Unit = {
        while(this.side == other.side) {
          switchSide()
          Thread.sleep(1000)
        }
      }
    }

    def deadlock(): Unit = {
      val sam = Friend("Sam")
      val pierre = Friend("Pierre")

      // sam's lock, then pierre's lock
      new Thread(() => sam.bow(pierre)).start()

      // pierres'lock, then sam's lock
      new Thread(() => pierre.bow(sam)).start()

    }

    def livelock(): Unit = {
      val sam = Friend("Sam")
      val pierre = Friend("Pierre")

      // no locks, but execution is yielded to each other
      new Thread(() => sam.pass(pierre)).start()
      new Thread(() => pierre.pass(sam)).start()
    }
  }

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()

      while (true)
        // all threads but one will be blocked
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[Consumer $id] buffer empty, waiting...")
            buffer.wait()
          }

          // there must be at least one value in the buffer
          val x = buffer.dequeue()
          println(s"[Consumer $id] consumed " + x)

          // signals one producer about available space
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0

      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println(s"[Producer $id] buffer is full, waiting...")
            buffer.wait()
          }

          // there must be at least one empty space in buffer
          println(s"[Producer $id] producing " + i)
          buffer.enqueue(i)
          buffer.notify()

          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }

}
