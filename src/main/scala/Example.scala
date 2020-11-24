import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Main {

  def main(args: Array[String]): Unit = {
    trait user {
      def a: String
    }
    trait account {
      def b: Int
    }

    case class User(a: String) extends user
    case class Account(b: Int) extends account

    trait userService {
      def userService: user
    }
    trait accountService {
      def AccountService: account
    }

    case class Services(userService: user, AccountService: account) extends userService with accountService

    object service {
      def get(a: String) = Arrow((n:userService) => Future(Right(n.userService.a)))
    }
    val x = Arrow.resolve[accountService, Int](5)
      .flatMap(
        (a) => service.get(a.toString)
      )
      .map(a => {
      println("test")
      a
    })
    x.groupParallel(x).runAsFuture(Services(User("yo"), Account(4)))
  }

  def msg = "Arrow"

}
