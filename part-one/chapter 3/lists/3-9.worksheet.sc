sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def length[A](as: List[A]): Int = 
        as match {
            case Nil => 0
            case Cons(_, Nil) => 1
            case Cons(_, t) => 1 + length(t) 
        }

    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1,2,3)
List.length(x)
