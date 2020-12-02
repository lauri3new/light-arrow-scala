
import org.scalatest.funsuite._
import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }

class SafetyTest extends AsyncFunSuite {
  test("Arrow map should be stack safe") {
    var a = Arrow.resolve(1)
    for (i <- Range(0, 10000)) {
      a = a.map(b => b + 1)
    }
    a
    .runAsFuture(null)
    .map {
      case Right(r) => assert(r == 10001)
    }
  }
  test("Arrow flatMap should be stack safe") {
    def a(n: Int): Arrow[Any, Nothing, Int] = {
      if (n == 1) {
        Arrow.resolve(1)
      } else {
        Arrow.resolve(1).flatMap(_ => a(n - 1))
      }
    }
    
    a(100000)
    .runAsFuture(null)
    .map {
      case Right(r) => assert(r == 1)
    }
  }

  test("Arrow construct should be stack safe") {
    def a(n: Int): Arrow[Any, Nothing, Int] = {
      if (n == 1) {
        Arrow.construct((resolve) => resolve(1))
      } else {
        Arrow.construct((resolve) => resolve(1)).flatMap(_ => a(n - 1))
      }
    }
    
    a(10)
    .runAsFuture(null)
    .map {
      case Right(r) => assert(r == 1)
    }
  }
  
  test("Arrow should construct - 1") {
    Arrow.construct[Nothing, Int]((resolve, reject) => {
      resolve(5)
    })
    .map(a => a + 1)
    .runAsFuture(null)
    .map {
      case Right(r) => assert(r == 6)
    }
  }
  test("Arrow should construct - 2") {
    Arrow.construct[Nothing, Int]((resolve, reject) => {
      sleep(100)(resolve(5))
    })
    .map(a => a + 1)
    .runAsFuture(null)
    .map {
      case Right(r) => assert(r == 6)
    }
  }
}