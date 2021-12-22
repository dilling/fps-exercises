sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def init[A](l: List[A]): List[A] = {
        l match {
            case Cons(_, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
            case Nil => Nil
        }
    }

    def apply[A](as :A*): List[A] =
       if(as.isEmpty) Nil
       else Cons(as.head, apply(as.tail: _*))
}

val x = List(1,2,3,4,5)
List.init(x)
