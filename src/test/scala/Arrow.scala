
import org.scalatest.funsuite._
import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }

class ArrowTest extends AsyncFunSuite {
  test("Arrow should map") {
    Arrow.resolve(1)
      .map(a => a * 3)
      .runAsFuture(null)
      .map {
        case Right(r) => assert(r == 3)
      }
    }
  test("Arrow should be immutable - a") {
    val a = Arrow.resolve(1)
    val b = a.map(x => x * 2)
    val c = a.map(x => x * 3)
    b.runAsFuture(null)
    .map {
      case Right(r) => assert(r == 2)
    }
  }
  test("Arrow should be immutable - b") {
    val a = Arrow.resolve(1)
    val b = a.map(x => x * 2)
    a.map(x => x * 3)
      .runAsFuture(null)
      .map {
        case Right(r) => assert(r == 3)
      }
  }
}