sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
// Fold right is kind of like the apply constructor for the list object, in that
// it has a repeated function with a default value termination