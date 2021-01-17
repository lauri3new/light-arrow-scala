package algebra

// motivating example

val x = List("bob").concat("loblaw")
val y = "bob" + "loblaw"

def repeatString (n: Int)(s: String): String = {
  def repeat(n2: Int)(s2: String): String = {
    if (n2 <= 1) {
      s2
    } else {
      repeat(n2 - 1)(s2 + s)
    }
  }
  repeat(n)(s)
}

def repeatList [A](n: Int)(as: List[A]): List[A] = {
  def repeat(n2: Int)(as2: List[A]): List[A] = {
    if (n2 <= 1) {
      as2
    } else {
      repeat(n2 - 1)(as.concat(as2))
    }
  }
  repeat(n)(as)
}

object Main {
  def main(args: Array[String]): Unit = {
    println(repeatString(5)("ok"))
    println(repeatList(5)(List(2)))
  }
}

// Monoids : things we can combine together

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

// WHERE empty should hold true for this law:
// combine(empty, A) === combine(empty, a) === a
// and associative: combine(combine(A, B), C) === combine(A, combine(B, C))

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

given [A]: Monoid[List[A]] with
  def combine (x:List[A], y: List[A]): List[A] = x.concat(y)
  def empty: List[A] = List()

// Monoid combinators (combinator = function with no free variables)
// this is the payoff :)

def repeat[T: Monoid](n:Int)(x: T) = {
  def repeat(n2: Int)(y: T): T = {
    if (n2 <= 1) {
      y
    } else {
      repeat(n2 - 1)(Monoid[T].combine(y, x))
    }
  }
  repeat(n)(x)
}
   
def combineAll[T: Monoid](
  xs: List[T]
): T = xs.foldLeft(Monoid[T].empty)((x, y) => Monoid[T].combine(x, y))


// val strings = List("yo", "wasup", "hi")
// val numbers = List(1, 5, 3)

// val myString = combineAll(strings)
// val myNumber = combineAll(numbers)

// object Main {
//   def main(args: Array[String]): Unit = {
//     println(repeatString(5)("ok"))
//     println(repeatList(5)(List(2)))
//   }
// }