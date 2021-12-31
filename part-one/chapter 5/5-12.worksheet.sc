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
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match {
            case Some((a: A, s: S)) => cons(a, unfold(s)(f))
            case _ => Empty
        }

    def ones(): Stream[Int] =
        unfold(1)(v => Some((v, 1)))
    
    def constant(n: Int): Stream[Int] =
        unfold(n)(v => Some((v, v)))
    
    def from(n: Int): Stream[Int] =
        unfold(n)(v => Some((v, v + 1)))
    
    def fibs(): Stream[Int] =
        unfold((0, 1))(s => Some(s._1, (s._2, (s._2 + s._1))))
    
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

Stream.ones().take(3).toList
Stream.constant(4).take(3).toList
Stream.from(4).take(3).toList
Stream.from(4).take(3).toList
Stream.fibs().take(8).toList

