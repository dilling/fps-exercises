import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def addLists(a1: List[Int], a2: List[Int]): List[Int] =
        (a1, a2) match {
            case (Nil, Nil) => Nil
            case (Cons(x1, xs1), Nil) => a1
            case (Nil, Cons(x1, xs1)) => a2
            case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, addLists(xs1, xs2))
        }
    
    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1, 2, 3)
val y = List(4, 5, 6)
List.addLists(x, y)

