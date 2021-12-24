import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def zipWith[A,B,C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] =
        (a1, a2) match {
            case (Nil, Nil) => Nil
            case (Cons(x1, xs1), Nil) => Nil
            case (Nil, Cons(x1, xs1)) => Nil
            case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
        }
    
    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1, 2, 3)
val y = List(4, 5, 6)
List.zipWith(x, y)(_ + _)

