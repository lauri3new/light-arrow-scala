package algebra

// Monoids : things we can combine together

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

// WHERE empty should hold true for this law:
// combine(empty, A) === combine(empty, a) === a
// can't encode this to be checked by compiler in Scala, but could do in other languages

// scala 3 stuff
object Monoid {
  def apply[T](using m: Monoid[T]) = m
}

// Type class instances

given Monoid[String] with
  def combine (x:String, y: String): String = x.concat(y)
  def empty: String = ""

given Monoid[Int] with
  def combine (x:Int, y: Int): Int = x + y
  def empty: Int = 0

// Monoid combinators (combinator = function with no free variables)
// this is the payoff :)
   
def combineAll[T: Monoid](
  xs: List[T]
): T = xs.foldLeft(Monoid[T].empty)((x, y) => Monoid[T].combine(x, y))

val strings = List("yo", "wasup", "hi")
val numbers = List(1, 5, 3)

val myString = combineAll(strings)
val myNumber = combineAll(numbers)

object Main {
  def main(args: Array[String]): Unit = {
    println(myString)
    println(myNumber)
  }
}

