package internal
import lightarrow.{ Arrow }
import scala.concurrent.Future

sealed trait Operations

final case class Value[R](val f: R) extends Operations

final case class FutureBased[D, E, R](val f: D => Future[Either[E, R]]) extends Operations

final case class Map[R, R1](val f: R => R1) extends Operations

final case class FlatMap[R, D, E, E2, R2](val f: R => Arrow[D, E | E2, R2]) extends Operations

final case class LeftMap[R, R1](val f: R => R1) extends Operations

final case class OrElse[D, E, R](val f: Arrow[D, E, R]) extends Operations

final case class AndThen[D, E, R](val f: Arrow[D, E, R]) extends Operations

final case class Group[D, E, R](val f: Arrow[D, E, R]) extends Operations

final case class GroupParallel[D, E, R, D2, E2, R2](val f: (Arrow[D, E, R], Arrow[D2, E2, R2])) extends Operations

final case class GroupFirst[D, E, R](val f: Arrow[D, E, R]) extends Operations

final case class GroupSecond[D, E, R](val f: Arrow[D, E, R]) extends Operations

final case class All[D, E, R](val f: Array[Arrow[D, E, R]]) extends Operations

final case class Race[D, E, R](val f: Array[Arrow[D, E, R]]) extends Operations

final case class Bracket[D, D2, E, R](val f: R => Arrow[D, E, Any], val g: R => Arrow[D2, E, R]) extends Operations
// type bracket = {
//   _tag: Ops.bracket
//   f: [(_:any) => Arrow<any, any, any>, (_:any) => Arrow<any, any, any>]
// }

// type construct = {
//   _tag: Ops.construct,
//   f: (_: any) => (resolve: (_: any) => void, reject: (_: any) => void) => void | (() => void)
// }
