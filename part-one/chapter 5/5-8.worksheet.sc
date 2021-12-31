sealed trait Stream[+A] {
    def toList: List[A] = 
        this match {
            case Empty => Nil
            case Cons(h, t) => h() :: t().toList
        }

    def take(n: Int): Stream[A] = 
        this match {
            case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
            case _ => Empty
        }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def constant[A](a: A): Stream[A] = {
        lazy val ax: Stream[A] = cons(a, ax)
        ax
    }
    
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = 
        if(as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
}

Stream.constant(3).take(3).toList