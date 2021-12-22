sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match {
            case Nil => Nil
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => l
        }
    }

    def apply[A](as :A*): List[A] =
       if(as.isEmpty) Nil
       else Cons(as.head, apply(as.tail: _*))
}

val x = List(1,2,3,4,5)
List.dropWhile(x, _ <= 3)