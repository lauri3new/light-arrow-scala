import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Main {

  def main(args: Array[String]): Unit = {
    trait user {
      def a: String
    }
    trait noob {
      def b: Int
    }

    case class User(a: String) extends user
    case class Noob(b: Int) extends noob

    enum Bla {
      case X, Y
    }

    trait userService {
      def userService: user
    }
    trait noobService {
      def noobService: noob
    }

    case class Services(userService: user, noobService: noob) extends userService with noobService

    object service {
      def get(a: String) = Arrow((n:userService) => Future(Right(n.userService.a)))
    }
    val x = Arrow.resolve[noobService, Int](5)
      .flatMap(
        (a) => service.get(a.toString)
      )
      .map(a => {
      println("huh")
      a
    }).runAsFuture(Services(User("yo"), Noob(4)))
    val y = Future(5)
    println("Hello world!")
  }

  def msg = "Arrow"

}
