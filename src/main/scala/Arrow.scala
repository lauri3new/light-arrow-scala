package lightarrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import internal._
import scala.util.{ Success, Failure }

object Arrow {

  // TODO: add combinators

  def apply[D, E, R](f: D => Future[Either[E, R]]) = new Arrow[D, E, R](List(FutureBased(f)))

  def resolve[R](f: R) = new Arrow[Any, Nothing, R](List(Value(f)))

  def construct[E, R](f: ((R => Unit), (E => Unit)) => Unit) = new Arrow[Any, E, R](List(Construct(f)))

  def construct[E, R](f: ((R => Unit)) => Unit) = new Arrow[Any, E, R](List(ConstructRes(f)))

  def reject[R](f: R) = new Arrow[Any, R, Nothing](List(LeftValue(f)))

  // def all[D, E, R](f: Array[Arrow[D, E, R]]) = new Arrow[D, E, Array[R]](List(All(f)))

  // def race[D, E, R](f: Array[Arrow[D, E, R]]) = new Arrow[D, E, R](List(Race(f)))

}

class Arrow[-D, +E, +R] private (val ops: List[Operations]) {

  def map[R2](f: R => R2) = new Arrow[D, E, R2](ops :+ Map(f))

  def flatMap[D2 <: D, E2, R2](f: R => Arrow[D2, E2, R2]) = new Arrow[D2, E | E2, R2](ops :+ FlatMap(f))

  def group[D2 <: D, E2, R2](f: Arrow[D2, E2, R2]) = new Arrow[D2, E | E2, (R, R2)](ops :+ Group(f))

  def groupParallel[D2 <: D, E2, R2](f: Arrow[D2, E2, R2]) = new Arrow[D2, E | E2, (R, R2)](List(GroupParallel(this, f)))

  def groupFirst[D2 <: D, E2, R2](f: Arrow[D2, E2, R2]) = new Arrow[D2, E | E2, R](ops :+ GroupFirst(f))

  def groupSecond[D2 <: D, E2, R2](f: Arrow[D2, E2, R2]) = new Arrow[D2, E | E2, R2](ops :+ GroupSecond(f))

  def bracket[D2 <: D, D3 <: D2, E2, R2](f: R => Arrow[D2, Nothing, Any], g: R => Arrow[D3, E2, R2]) = new Arrow[D3, E | E2, R2](ops :+ Bracket(f, g))

  def biMap[E2, R2](f: E => E2, g: R => R2) = new Arrow[D, E2, R](ops :+ LeftMap(f) :+ Map(g))

  def leftMap[E2](f: E => E2) = new Arrow[D, E2, R](ops :+ LeftMap(f))

  def orElse[D2 <: D, E2, R2](f: Arrow[D2, E2, R2]) = new Arrow[D2, E2, R | R2](ops :+ OrElse(f))

  def andThen[E2, R2](f: Arrow[R, E2, R2]) = new Arrow[D, E | E2, R2](ops :+ AndThen(f))

  def runAsCFuture(d: D): Cancellable[Either[E, R]] = Runner[D, E, R](d, ops).run

  def runAsFuture(d: D): Future[Either[E, R]] = Runner[D, E, R](d, ops).run.future

  // TODO: execution context with default param?
  def run(
    dependencies: D,
    f: R => Unit,
    g: E => Unit,
    h: Throwable => Unit
  ): () => Unit = {
    val CFuture = Runner[D, E, R](dependencies, ops).run
    var _cancel = false
    CFuture.future.onComplete {
      case Success(eitherR) => {
        if (!_cancel) {
          eitherR match {
            case Right(r) => f(r)
            case Left(e) => g(e)
          }
        }
      }
      case Failure(t) => {
        if (!_cancel) {
          h(t)
        }
      }
    }
    def cancel() = {
      CFuture.unsafeCancel()
      _cancel = true
    }
    cancel
  }

}