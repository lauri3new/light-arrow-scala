package lightarrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import Either._
import Right._

object Arrow {
  def apply[D, E, R](v: D => Future[Either[E, R]]) = new Arrow(v)

  def resolve[D, R](v: R) = new Arrow((d: D) => Future(Right(v)))

  def reject[D, R](v: R) = new Arrow((d: D) => Future(Left(v)))
}

class Arrow[-D, E, R](val v: D => Future[Either[E, R]]) {
  def map[R2](f: R => R2) = Arrow((d:D) => v(d).map(ea => ea.map(f)))

  def flatMap[D2 <: D, E1, R2](f: R => Arrow[D2, E1, R2]) = Arrow((d:D2) => v(d).flatMap(
  (ea) => ea match {
    case Left(value) => Future(Left(value))
    case Right(value) => f(value).v(d)
    }
  ))

  def leftMap[E2](f:E => E2) = Arrow((d:D) => v(d).map(ea => ea.left.map(f)))

  def biMap[E2, R2](f:E => E2, g: R => R2) = Arrow((d:D) => v(d).map(ea => ea match {
    case Left(value) => Left(f(value))
    case Right(value) => Right(g(value))
  }))

  def orElse[D2 <: D, E2, R2](f:Arrow[D2, E2, R2]) = Arrow((d:D2) => v(d).flatMap(ea => ea match {
    case Left(value) => f.v(d)
    case Right(value) => Future(Right(value))
  }))

  def andThen[R2, E1](f: Arrow[R, E1, R2]) = Arrow((d:D) => v(d).flatMap(
    (ea) => ea match {
      case Left(value) => Future(Left(value))
      case Right(value) => f.v(value)
    }
  ))

  def group[D2 <: D, E2, R2](f:Arrow[D2, E2, R2]) = Arrow((d:D2) => v(d).flatMap(
    (ea) => ea match {
      case Left(value) => Future(Left(value))
      case Right(value) => f.v(d).map(eb => eb.map(r => (value, r)))
    }
  ))

  def groupFirst[D2 <: D, E2, R2](f:Arrow[D2, E2, R2]) = Arrow((d:D2) => v(d).flatMap(
    (ea) => ea match {
      case Left(value) => Future(Left(value))
      case Right(value) => f.v(d)
    }
  ))

  def groupSecond[D2 <: D, E2, R2](f:Arrow[D2, E2, R2]) = Arrow((d:D2) => v(d).flatMap(
    (ea) => ea match {
      case Left(value) => Future(Left(value))
      case Right(value) => f.v(d).map(eb => eb.map(r => value))
    }
  ))

  def groupParallel[D2 <: D, E2, R2](f:Arrow[D2, E2, R2]): Arrow[D2, E | E2, (R, R2)] = Arrow((d:D2) => {
    val a = v(d)
    val b = f.v(d)
    val x = for {
      a1 <- a
      b1 <- b
    } yield ((a1, b1))
    x.map(
      (c, d) => c.flatMap(r1 => d.map(r2 => (r1, r2))) 
    )
  })

  def flatMapF[D2 <: D, E1, R2](f: R => D2 => Future[Either[E, R]]) = Arrow((d:D2) => v(d).flatMap(
    (ea) => ea match {
      case Left(value) => Future(Left(value))
      case Right(value) => f(value)(d)
    }
  ))

  def runAsFuture(d:D) = v(d)
}