import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    def sum(ns: List[Double]) =
        foldLeft(ns, 1.0)(_ + _)

    def product(ns: List[Double]) =
        foldLeft(ns, 1.0)(_ * _)
    
    def length[A](ns: List[A]) = 
        foldLeft(ns, 0)((z,_) => z + 1)
    

    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1.0,2.0,3.0)
List.sum(x)
List.product(x)
List.length(x)