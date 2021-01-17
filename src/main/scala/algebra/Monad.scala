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

def forEach[T[_]: Monad, A](x: T[A])(f: A => Unit) = Monad[T].flatMap(x)(a => {
  f(a)
  Monad[T].of(a)
})
def map2[T[_]: Monad, A, B, Z](x: T[A], y: T[B])(f: (A, B) => Z) = Monad[T].flatMap(x)(a => Monad[T].map(y)(b => f(a, b)))

object MonadApp {
  def main(args: Array[String]): Unit = {
    println(map2(some("jim"), none)((a, b) => "hello " + a + b))
    val x = map2(FPFuture(() => Future(5)), FPFuture(() => Future(10)))((a, b) => a + b)
    forEach(x)(a => println(a)).run()
  }
}