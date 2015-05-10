package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.all success") {
    val all = Future.all(List(Future(1), Future(2), Future(3)))
    assert(Await.result(all, 10 millis) == List(1, 2, 3))
  }

  test("Future.all failure") {
    val all = Future.all(List(Future(1), Future(2), Future{throw new Exception}))
    assert(Await.result(all.recover{case _ => 0}, 10 millis) == 0)
  }

  test("Future.any") {
    (1 to 100).foreach { _ =>
      val any = Future.any(List(Future(1), Future(2), Future{throw new Exception}))
      val result = Await.result(any.recover{case _ => 0}, 10 millis)
      assert(Set(0, 1, 2)(result))
    }
  }

  test("Future.delay") {
    assert(Await.ready(Future.delay(1 second), 1.1 seconds).isCompleted)
    try {
      Await.result(Future.delay(1 second), 0.9 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("continueWith") {
    val f = Future(1).continueWith(_ => "foo")
    assert(Await.result(f, 10 millis) == "foo")
  }

  test("continue") {
    val f = Future(1).continue(_ => "foo")
    assert(Await.result(f, 10 millis) == "foo")
  }

  test("cancel") {
    var stack = List[Int]()
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          stack = 0 :: stack
        }
        stack = 1 :: stack
      }
    }
    Future.delay(10 millis) onSuccess {
      case _ => working.unsubscribe()
    }
    Thread.sleep(100)
    assert(stack.size > 10)
    assert(stack.head == 1)
    assert(stack.last == 0)
  }

  
  
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




