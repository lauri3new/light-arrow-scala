package algebra
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait Monad[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B) = this.flatMap(fa)(a => of(f(a)))
  def of[A](a:A): F[A]
}


// scala 3 stuff
object Monad {
  def apply[T[_]](using m: Monad[T]) = m
}

class FPFuture[A] (val a:() => Future[A]) {
  def flatMap[B](b:A => FPFuture[B]) = FPFuture(() => a().flatMap(a => b(a).a()))
  def run(): Future[A] = a()
}

given Monad[List] with
  def flatMap[A, B](a: List[A])(f: A => List[B]) = a.flatMap(f)
  def of[A](a:A) = List(a)

given Monad[Option] with
  def flatMap[A, B](a: Option[A])(f: A => Option[B]) = a.flatMap(f)
  def of[A](a:A) = Some(a)

given Monad[FPFuture] with
  def flatMap[A, B](a: FPFuture[A])(f: A => FPFuture[B]) = a.flatMap(f)
  def of[A](a:A) = FPFuture(() => Future(a))

def some[A](a:A): Option[A] = Some(a)
def none[A]: Option[A] = None

// combinators

extension [T[_]: Monad, A, B, Z](x: T[A])
  def forEach(f: A => Unit) = Monad[T].flatMap(x)(a => {
    f(a)
    Monad[T].of(a)
  })
  def map2(y: T[B])(f: (A, B) => Z) = Monad[T].flatMap(x)(a => Monad[T].map(y)(b => f(a, b)))


object MonadApp {
  def main(args: Array[String]): Unit = {
    val x = some("jim").map2(none)((a, b) => "hello " + a + b)
    println(x)
    val y = FPFuture(() => Future(5)).map2(FPFuture(() => Future(10)))((a, b) => a + b)
    val z = y.forEach(a => println(a))
    z.run() // run effect
  }
}