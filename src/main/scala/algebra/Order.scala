package algebra

// intro

// an interface that gives useful inferences
// given that A is a thing we can derive that we can do these things with A
// law-abiding instances
// derived operations

// To write generic code we try to maximise number of instances and derived operations
// however often these trade against each other

enum Ord {
  case LT, GT, EQ
}

trait Order[A] {
  def compare(x:A, y:A): Ord
}

// scala 3 stuff
object Order {
  def apply[T](using m: Order[T]) = m
}

// Instances
given Order[Int] with
  def compare (x:Int, y: Int): Ord = {
    if (x == y) then Ord.EQ else if (x > y) then Ord.GT else Ord.LT 
  }

enum Suit {
  case Spade, Heart, Club, Diamond
}

given Order[Suit] with
  def compare (x:Suit, y: Suit): Ord = {
    (x, y) match {
      case (Suit.Spade, Suit.Spade) => Ord.EQ
      case (Suit.Spade, _) => Ord.GT
      case (Suit.Heart, Suit.Spade) => Ord.LT
      case (Suit.Heart, Suit.Heart) => Ord.EQ
      case (_, Suit.Heart) => Ord.LT
      case (Suit.Club, Suit.Diamond) => Ord.GT
      case (Suit.Club, Suit.Club) => Ord.EQ
      case (Suit.Club, _) => Ord.LT
      case (Suit.Diamond, Suit.Diamond) => Ord.EQ
      case (Suit.Diamond, _) => Ord.LT
    }
  }

// combinators

def maximum[A: Order](x: A, y: A) = if (Order[A].compare(x, y) == Ord.GT) then x else y
def minimum = ???
object OrderApp {
  def main(args: Array[String]): Unit = {
    println(maximum(Suit.Heart, Suit.Spade))
    println(maximum(5, 10))
  }
}