sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def setHead[A](xs: List[A], x: A): List[A] = xs match {
        case Nil => List(x)
        case Cons(_, t) => Cons(x, t)
    }

    def apply[A](as :A*): List[A] =
       if(as.isEmpty) Nil
       else Cons(as.head, apply(as.tail: _*))
}

val x = List(1,2,3)
List.setHead(x, 4)