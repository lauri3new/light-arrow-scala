package algebra

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// scala 3 stuff
object Functor {
  def apply[T[_]](using m: Functor[T]) = m
}

given Functor[List] with
  def map[A, B](a: List[A])(f: A => B) = a.map(f)

// combinators

def product[T[_]: Functor, A, B](x: T[A])(f: A => B) = Functor[T].map(x)(a => (a, f(a)))

object FunctorApp {
  def main(args: Array[String]): Unit = {
    println(product(List("jim", "jon"))(a => "hello " + a))
  }
}