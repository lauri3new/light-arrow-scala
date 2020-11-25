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
          case _ => {}
        }
      }
    }
    _run
    Cancellable(this.cancel, p.future)
  }
}