package internal
import scala.annotation.tailrec
import scala.collection.mutable.{ Stack }
import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

// TODO: add some kind of truly cancellable Future
case class Cancellable[+A](unsafeCancel: () => Unit, future: Future[A])
 
// TODO: think about exception handling
class Runner[D, E, R](context: D, operations: List[Operations]) {
  // TODO: cancellation array
  // TODO: refactor mutations to methods
  private var _cancel = false
  private var stack = operations.to(Stack)
  private var result: Any = null
  private var isLeft: Boolean = false
  private var error: Any = null

  def cancel(): Unit = {
    _cancel = true
  }

  def runConstructRes(f: (Any => Unit) => Unit) = {
    val p = Promise[Any]()
    def resolve(a: Any) = {
      result = a
      p.success(null)
    }
    f(resolve)
    p.future
  }

  def runConstruct(f: (Any => Unit, Any => Unit) => Unit) = {
    val p = Promise[Any]()
    def resolve(a: Any) = {
      result = a
      p.success(null)
    }
    def reject(a: Any) = {
      isLeft = true
      error = a
      p.success(null)
    }
    f(resolve, reject)
    p.future
  }

  def run: Cancellable[Either[E, R]] = {
    val p = Promise[Either[E, R]]()
    def _run: Future[Any] = Future {
      try {
        if (_cancel) {
          p.success(Right(null.asInstanceOf[R]))
        }
        if (stack.isEmpty) {
          if (isLeft) {
            p.success(Left(error.asInstanceOf[E]))
          } else {
            p.success(Right(result.asInstanceOf[R]))
          }
        }
        val op = stack.pop()
        if (isLeft) {
          op match {
            case LeftMap(f): LeftMap[Any, Any] => {
              error = f(error)
              _run
            }
            case OrElse(f): OrElse[Any, Any, Any] => {
              isLeft = false
              stack.pushAll(f.ops)
              _run
            }
            case _ => _run
          }
        }
        op match {
          case Value(f) => {
            result = f
            _run
          }
          case LeftValue(f) => {
            isLeft = true
            error = f
            _run
          }
          case Map(f): Map[Any, Any] => {
            result = f(result)
            _run
          }
          case FlatMap(f): FlatMap[Any, Any, Any, Any, Any] => {
            stack.pushAll(f(result).ops)
            _run
          }
          case Construct(f): Construct[Any, Any] => {
            runConstruct(f).onComplete {
              _ => {
                _run 
              }
            }
          }
          case ConstructRes(f): ConstructRes[Any, Any] => {
            runConstructRes(f).onComplete {
              _ => {
                _run
              }
            }
          }
          case FutureBased(f): FutureBased[Any, Any, Any] => {
            f(context).onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case Group(f): Group[Any, Any, Any] => {
            f.runAsCFuture(context).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = (result, a)
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case GroupParallel(f): GroupParallel[Any, Any, Any, Any, Any, Any] => {
            val xx = for {
              ea: Either[Any, Any] <- f(0).runAsCFuture(context).future
              eb: Either[Any, Any] <- f(1).runAsCFuture(context).future
            } yield (ea.flatMap(a => eb.flatMap(b => Right(b, a))))
            xx.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = (result, a)
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case GroupFirst(f): GroupFirst[Any, Any, Any] => {
            f.runAsCFuture(context).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case GroupSecond(f): GroupSecond[Any, Any, Any] => {
            f.runAsCFuture(context).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case AndThen(f): AndThen[Any, Any, Any] => {
            f.runAsCFuture(result).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case Bracket(f, g): Bracket[Any, Any, Any, Any, Any] => {
            g(result).runAsCFuture(context).future.onComplete(ma => { ma match {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
            f(result).runAsCFuture(context).future.onComplete {
              case Success(_) => _run
              case Failure(t) => p.failure(t)
            }
          })
          }
          case _ => _run
        }
      } catch {
       case e: Throwable => p.failure(e)
      }
    }
    _run
    Cancellable(this.cancel, p.future)
  }
}