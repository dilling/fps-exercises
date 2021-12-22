sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    
    def product(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)

    def apply[A](as :A*): List[A] =
       if(as.isEmpty) Nil
       else Cons(as.head, apply(as.tail: _*))
}

val x = List(1.0, 2.0, 3.0, 4.0)

List.product(x)

// Answer
// If we could lazy evaluate fold right perhaps, but don't see how without modifying the foldRight function

