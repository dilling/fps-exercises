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
    
    def reverse[A](ns: List[A]): List[A] = 
        foldLeft(ns, Nil: List[A])((x, y) => Cons(y,x))
    
    def foldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
        foldLeft(reverse(as), z)(f)

    def append[A](a1: List[A], a2: List[A]): List[A] =
        foldRight(a1, a2)((z,x) => Cons(x,z))

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        foldRight(as, Nil: List[B])((z, x) => append(f(x), z))
    
    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1, 2, 3)
List.flatMap(x)(i => List(i,i))
