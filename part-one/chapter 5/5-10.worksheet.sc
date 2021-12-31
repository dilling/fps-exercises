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
    def fibs(): Stream[Int] = {
        def go(prev: Int, curr: Int): Stream[Int] =
            cons(prev, go(curr, curr + prev))

        go(0,1)
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

Stream.fibs().take(10).toList