// import lightarrow.Arrow
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readLine
import scala.util.{ Try, Random }

object Game {

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]
    extension [A, B](fa: F[A])
      def flatMap(afb: A => F[B]): F[B]
      def map(ab: A => B): F[B]
      def andThen(fb: F[B]): F[B]
  }

  object IO {
    def apply[A](a: => () => A): IO[A] = new IO(a)
    def point[A](a: => A) = new IO(() => a)
  }

  class IO[A](val a: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(a()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(a()).unsafeRun())
    def andThen[B](b: IO[B]): IO[B] = this.flatMap((_: A) => b)
    def unsafeRun(): A = a()
  }

  trait Console[F[_]] {
    def printLn(s: String): F[Unit]
    def readLn: F[String]
  }

  object Console {
    def apply[T[_]](using m: Console[T]) = m
  }

  object Program {
    def apply[T[_]](using m: Program[T]) = m
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
  given Program[IO] with
    def finish[A](a: => A) = IO.point(a)
    extension [A, B](fa: IO[A])
      def flatMap(afb: A => IO[B]) = fa.flatMap(afb)
      def map(ab: A => B): IO[B] = fa.map(ab)
      def andThen(fb: IO[B]) = fa.flatMap(a => fb)
  
  given Console[IO] with
    def printLn(s: String) = IO.point(println(s))
    def readLn = IO.point(readLine)

  
  def printLn[F[_]: Console](s: String) = Console[F].printLn(s)
  def readLn[F[_]: Console] = Console[F].readLn
  def nextInt[F[_]: Program] = Program[F].finish(Random.nextInt(5) + 1)

  def checkContinue[F[_]: Console : Program](): F[Boolean] = printLn("would you like to continue?")
    .andThen(
      Console[F].readLn.map(a => a.toLowerCase())
    ).flatMap(
      a => a match {
        case "y" => Program[F].finish(true)
        case "n" => Program[F].finish(false)
        case _ => checkContinue()
      }
    )

  def gameLoop[F[_]: Program : Console](name: String): F[Unit] = {
    def stuff(exec: Boolean): F[Unit] = {
      if (exec == false) {
        printLn("bye!")
      } else {
        printLn("dear " + name + ", please guess a number from 1 to 5:")
        .andThen(readLn)
        .map(parseInt)
        .flatMap(
          guess => guess match {
            case None => printLn("you didn't enter a number!")
            case Some(guess) => nextInt.chain(num => {
              if (guess == num) printLn("you guessed right " + name + "!")
              else printLn("you guessed wrong, the number was " + num)
            })
          }
        ).andThen(
          checkContinue()
        ).flatMap(
          cont => if (cont) stuff(true) else stuff(false)
        )
      }
    }
    stuff(true)
  }

  def mainIO[F[_]: Console: Program] = {
    for {
      _ <- printLn("hello, what is your name?")
      name <- readLn
      _ <- printLn("yo, " + name + ", welcome to the game!").andThen(gameLoop(name))
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    mainIO[IO].unsafeRun()
  }

  // trait Program[F[_]] {
  //   def finish[A](a: => A): F[A]
  //   extension [A, B](fa: F[A])
  //     def chain(afb: A => F[B]): F[B]
  //     def map(ab: A => B): F[B]
  // }

  // trait Console[F[_]] {
  //   def printLn(s: String): F[Unit]
  //   def readLn: F[String]
  // }

  // object Program {
  //   def apply[T[_]](using m: Program[T]) = m
  // }

  // given Program[IO] with
  //   def finish[A](a: => A) = IO(() => a)
  //   extension [A, B](fa: IO[A])
  //     def chain(afb: A => IO[B]) = fa.flatMap(afb)
  //     def map(ab: A => B): IO[B] = fa.map(ab)

  // object IO {
  //   def apply[A](a: => () => A): IO[A] = new IO(a)
  // }

  // class IO[A](val a: () => A) {
  //   def map[B](f: A => B): IO[B] = IO(() => f(a()))
  //   def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(a()).unsafeRun())
  //   def andThen[B](b: IO[B]): IO[B] = this.flatMap((_: A) => b)
  //   def unsafeRun(): A = a()
  // }

  // def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  // def printLn(s: String) = IO(() => println(s))
  // def readLn = IO[String](() => readLine())
  // def nextInt = IO(() => Random.nextInt(5) + 1)

  // def checkContinue: IO[Boolean] = printLn("would you like to continue?")
  //   .andThen(
  //     readLn.map(a => a.toLowerCase())
  //   ).flatMap(
  //     a => a match {
  //       case "y" => IO(() => true)
  //       case "n" => IO(() => false)
  //       case _ => checkContinue
  //     }
  //   )

  // def gameLoop(name: String): IO[Unit] = {
  //   def stuff(exec: Boolean): IO[Unit] = {
  //     if (exec == false) {
  //       printLn("bye!")
  //     } else {
  //       printLn("dear " + name + ", please guess a number from 1 to 5:")
  //       .andThen(readLn)
  //       .map(parseInt)
  //       .flatMap(
  //         guess => guess match {
  //           case None => printLn("you didn't enter a number!")
  //           case Some(guess) => nextInt.flatMap(num => {
  //             if (guess == num) printLn("you guessed right " + name + "!")
  //             else printLn("you guessed wrong, the number was " + num)
  //           })
  //         }
  //       ).andThen(
  //         checkContinue
  //       ).flatMap(
  //         cont => if (cont) stuff(true) else stuff(false)
  //       )
  //     }
  //   }
  //   stuff(true)
  // }

  // def main(args: Array[String]): Unit = {
  //   printLn("hello, what is your name?")
  //   .andThen(readLn)
  //   .flatMap(name => printLn("yo, " + name + ", welcome to the game!")
  //     .andThen(gameLoop(name))
  //   )
  //   .unsafeRun()
  // }

  // def main(args: Array[String]): Unit = {
  //   println("hello, what is your name?")

  //   val name = readLine()

  //   println("yo, " + name + ", welcome to the game!")

  //   var exec = true

  //   while(exec) {
  //     val num = Random.nextInt(5) + 1

  //     println("dear " + name + ", please guess a number from 1 to 5:")

  //     val guess = parseInt(readLine())

  //     guess match {
  //       case None => println("you didn't enter a number!")
  //       case Some(guess) =>
  //         if (guess == num) println("you guessed right " + name + "!")
  //         else println("you guessed wrong, the number was " + num)
  //     }
      
  //     var cont = true

  //     while (cont) {
  //       cont = false
  //       println("do you want to continue?")

  //       readLine() match {
  //         case "y" => {
  //           exec = true
  //         }
  //         case "n" => {
  //           exec = false
  //         }
  //         case _ => {
  //           cont = true
  //         }
  //       }
  //     }

  //   }

  // }

}
