package internal
import scala.collection.mutable.{ Stack }
import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global

// TODO: add some kind of truly cancellable Future
case class Cancellable[A](cancel: () => Unit, f: Future[A])

// TODO: think about exception handling
class Runner[D, E, R](context: D, operations: List[Operations]) {
  private var _cancel = false
  private var stack = operations.to(Stack)
  private var block = false
  private var result: Any = null
  private var isLeft: Boolean = false
  private var error: Any = null
  def cancel(): Unit = {
    _cancel = true
  }
  def run: Cancellable[Either[E, R]] = {
    val p = Promise[Either[E, R]]()
    def _run: Future[Any] = Future {
      if (stack.isEmpty) {
        if (isLeft) {
          p.success(Left(error.asInstanceOf[E]))
        } else {
          p.success(Right(result.asInstanceOf[R]))
        }
      }
      while (!stack.isEmpty && !block) {
        if (_cancel) {
          p.success(Right(null.asInstanceOf[R]))
        }
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
            // TODO: add internal error
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
          case FutureBased(f): FutureBased[Any, Any, Any] => {
            block = true
            f(context).map(ea => ea match {
              case Right(a) => {
                result = a
                block = false
                _run
              }
              case Left(e) => {
                error = e
                isLeft = true
                block = false
                _run
              }
            })
          }
          case Group(f): Group[Any, Any, Any] => {
            block = true
            f.run(context).f.foreach(ea => ea match {
              case Right(a) => {
                result = (result, a)
                block = false
                _run
              }
              case Left(e) => {
                error = e
                isLeft = true
                block = false
                _run
              }
            })
          }
          case GroupParallel(f): GroupParallel[Any, Any, Any, Any, Any, Any] => {
            block = true
            for {
              ea: Either[Any, Any] <- f(0).run(context).f
              eb: Either[Any, Any] <- f(1).run(context).f
            } yield {
              val xx = ea.flatMap(a => eb.flatMap(b => Right(b, a)))
              xx match {
              case Right(a) => {
                block = false
                _run
              }
              case Left(e) => {
                error = e
                isLeft = true
                block = false
                _run
              }
            }
            }
          }
          case GroupFirst(f): GroupFirst[Any, Any, Any] => {
            block = true
            f.run(context).f.foreach(ea => ea match {
              case Right(a) => {
                block = false
                _run
              }
              case Left(e) => {
                error = e
                isLeft = true
                block = false
                _run
              }
            })
          }
          case GroupSecond(f): GroupSecond[Any, Any, Any] => {
            block = true
            f.run(context).f.foreach(ea => ea match {
              case Right(a) => {
                result = a
                block = false
                _run
              }
              case Left(e) => {
                error = e
                isLeft = true
                block = false
                _run
              }
            })
          }
          case AndThen(f): AndThen[Any, Any, Any] => {
            block = true
            f.run(result).f.foreach(ea => ea match {
              case Right(a) => {
                result = a
                block = false
                _run
              }
              case Left(e) => {
                error = e
                isLeft = true
                block = false
                _run
              }
            })
          }
          case All(f): All[Any, Any, Any] => {
            block = true
            Future.sequence(
              f.map(a => a.run(context).f)
            )
            .map(
              aas => aas.foldLeft(Right(Array[Any]()))((eb: Either[Any, Array[Any]], ea: Either[Any, Any]) => ea.flatMap(a => eb.flatMap(b => Right(b :+ a))))
            )
            .foreach(aas => {
              result = aas
              block = false
              _run
            })
          }
          case Race(f): Race[Any, Any, Any] => {
            block = true
            Future.firstCompletedOf(
              f.map(a => a.run(context).f)
            )
            .foreach(aas => {
              result = aas
              block = false
              _run
            })
          }
          case Bracket(f, g): Bracket[Any, Any, Any, Any] => {
            block = true
            f(result).run(context).f.foreach(ea => {
              ea match {
                case Right(a) => {
                  result = a
                }
                case Left(e) => {
                  error = e
                  isLeft = true
                }
              }
              g(result).run(context).f.foreach((eb) => {
                block = false
                _run
              })
            })
          }
          case _ => { }
        }
      }
    }
    _run
    Cancellable(this.cancel, p.future)
  }
}