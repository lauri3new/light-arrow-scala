package algebra

// what is a type class?

trait Equal[A] {
  extension (a: A) def equalz(a2: A): Boolean
}

object Equal {
  def apply[A](using e: Equal[A]) = e
}

// type class instance

given Equal[Int] with
  extension (a: Int) def equalz (a2: Int): Boolean = a == a2

// operations
extension [A : Equal](a:A)
  def notEquals(a2: A) = !a.equalz(a2)

val x = 1.equalz(2)
// val y = "yo".equalz("he")
