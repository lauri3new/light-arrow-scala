package internal
import scala.annotation.tailrec
import scala.collection.mutable.{ Stack }
import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

// TODO: add some kind of truly cancellable Future
case class Cancellable[A](cancel: () => Unit, future: Future[A])

// TODO: think about exception handling
class Runner[D, E, R](context: D, operations: List[Operations]) {
  // TODO: cancellation array
  // TODO: refactor mutations to methods
  private var _cancel = false
  private var stack = operations.to(Stack)
  private var block = false
  private var result: Any = null
  private var isLeft: Boolean = false
  private var error: Any = null
  private var batchcount = 0
  def cancel(): Unit = {
    _cancel = true
  }

  def runConstruct(f: (Any => Unit, Any => Unit) => Unit, cb: => Future[Any]) = {
    var pending = true
    val resolve = (a: Any) => {
      result = a
      pending = false
    }
    val reject = (a: Any) => {
      isLeft = true
      error = a
      pending = false
    }
    f(resolve, reject)
    while (!pending) {
      batchcount += 1
    }
    pending = false
    block = true
    cb
  }

  def run: Cancellable[Either[E, R]] = {
    val p = Promise[Either[E, R]]()
    def _run: Future[Any] = Future {
      try {
      while (!block) {
        batchcount += 1
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
        if (batchcount > 2048) {
          block = false
          batchcount = 0
          _run
        } else {
        val op = stack.pop()
        if (isLeft) {
          op match {
            case LeftMap(f): LeftMap[Any, Any] => {
              error = f(error)
            }
            case OrElse(f): OrElse[Any, Any, Any] => {
              isLeft = false
              stack.pushAll(f.ops)
            }
            case _ => {}
          }
        }
        op match {
          case Value(f) => {
            result = f
          }
          case Map(f): Map[Any, Any] => {
            result = f(result)
          }
          case FlatMap(f): FlatMap[Any, Any, Any, Any, Any] => {
            stack.pushAll(f(result).ops)
          }
          case Construct(f): Construct[Any, Any] => {
            block = true
            runConstruct(f, _run)
          }
          case FutureBased(f): FutureBased[Any, Any, Any] => {
            block = true
            f(context).onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case Group(f): Group[Any, Any, Any] => {
            block = true
            f.runAsCFuture(context).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = (result, a)
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case GroupParallel(f): GroupParallel[Any, Any, Any, Any, Any, Any] => {
            block = true
            val xx = for {
              ea: Either[Any, Any] <- f(0).runAsCFuture(context).future
              eb: Either[Any, Any] <- f(1).runAsCFuture(context).future
            } yield (ea.flatMap(a => eb.flatMap(b => Right(b, a))))
            xx.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = (result, a)
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case GroupFirst(f): GroupFirst[Any, Any, Any] => {
            block = true
            f.runAsCFuture(context).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case GroupSecond(f): GroupSecond[Any, Any, Any] => {
            block = true
            f.runAsCFuture(context).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case AndThen(f): AndThen[Any, Any, Any] => {
            block = true
            f.runAsCFuture(result).future.onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case All(f): All[Any, Any, Any] => {
            block = true
            Future.sequence(
              f.map(a => a.runAsCFuture(context).future)
            )
            .onComplete {
              case Success(aas) => aas.foldLeft(Right(Array[Any]()))((eb: Either[Any, Array[Any]], ea: Either[Any, Any]) => ea.flatMap(a => eb.flatMap(b => Right(b :+ a)))) match {
                case Right(aaas) => {
                  result = aaas
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case Race(f): Race[Any, Any, Any] => {
            block = true
            Future.firstCompletedOf(
              f.map(a => a.runAsCFuture(context).future)
            )
            .onComplete {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
          }
          case Bracket(f, g): Bracket[Any, Any, Any, Any, Any] => {
            block = true
            f(result).runAsCFuture(context).future.onComplete(ma => { ma match {
              case Success(ea) => ea match {
                case Right(a) => {
                  result = a
                  block = false
                  batchcount = 0
                  _run
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                  block = false
                  batchcount = 0
                  _run
                }
              }
              case Failure(t) => p.failure(t)
            }
            g(result).runAsCFuture(context).future.onComplete {
              case Success(_) => _run
              case Failure(t) => p.failure(t)
            }
          })
          }
          case _ => { }
        }
      }
    }
    } catch {
        case e: Throwable => p.failure(e)
      }
    }
    _run
    Cancellable(this.cancel, p.future)
  }
}