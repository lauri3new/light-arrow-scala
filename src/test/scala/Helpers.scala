import scala.concurrent.{ Future, Promise }
import scala.util.Try
import java.util.{ Timer, TimerTask }
import java.util.Date

def sleep[T](delay: Long)(block: => T): Future[T] = {
  val promise = Promise[T]()
  val t = new Timer()
  t.schedule(new TimerTask {
    override def run(): Unit = {
      promise.complete(Try(block))
    }
  }, delay)
  promise.future
}