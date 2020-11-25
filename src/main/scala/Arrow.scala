package lightarrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import internal._

import Either._
import Right._

object Arrow {
  def apply[D, E, R](f: D => Future[Either[E, R]]) = new Arrow[D, E, R](List(FutureBased(f)))

  def resolve[D, R](f: R) = Arrow((d: D) => Future(Right(f)))

  def reject[D, R](f: R) = Arrow((d: D) => Future(Left(f)))
}

// TODO: fix typings

class Arrow[-D, E, R] private (val ops: List[Operations]) {
  def map[R2](f: R => R2) = new Arrow[D, E, R2](ops :+ Map(f))

  def flatMap[D2 <: D, E1, R2](f: R => Arrow[D2, E1, R2]) = new Arrow[D2, E | E1, R2](ops :+ FlatMap(f))

  def group[D2 <: D, E1, R2](f: Arrow[D2, E1, R2]) = new Arrow[D, E | E1, (R, R2)](ops :+ Group(f))

  def groupFirst[D2 <: D, E1, R2](f: Arrow[D2, E1, R2]) = new Arrow[D, E | E1, (R, R2)](ops :+ GroupFirst(f))

  def groupSecond[D2 <: D, E1, R2](f: Arrow[D2, E1, R2]) = new Arrow[D, E | E1, (R, R2)](ops :+ GroupSecond(f))

  def run(d: D): Cancellable[Either[E, R]] = Runner[D, E, R](d, ops).run

}