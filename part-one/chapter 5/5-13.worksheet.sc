sealed trait Stream[+A] {
    def toList: List[A] = 
        this match {
            case Empty => Nil
            case Cons(h, t) => h() :: t().toList
        }

    def map[B](f: A => B): Stream[B] = 
        Stream.unfold(this)(_ match {
            case Cons(h, t) => Some(f(h()), t())
            case _ => None
        })

    def take(n: Int): Stream[A] = 
        Stream.unfold((n, this))(_ match {
            case(n, Cons(h, t)) if (n > 0) => Some(h(), (n - 1, t()))
            case _ => None
        })

    def takeWhile(f: A => Boolean): Stream[A] =
        Stream.unfold(this)(_ match {
            case Cons(h, t) if f(h()) => Some(h(), t())
            case _ => None
        })

    def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
        Stream.unfold((this, b))(_ match {
            case (Cons(x, xs), Cons(y, ys)) => Some((f(x(), y()), (xs(), ys())))
            case _ => None
        })

    def zipAll[B](a2: Stream[B]): Stream[(Option[A], Option[B])] =
        Stream.unfold((this, a2))(_ match {
            case (Cons(x, xs), Cons(y, ys)) => Some(((Some(x()), Some(y())), (xs(), ys())))
            case (Cons(x, xs), Empty) => Some(((Some(x()), None), (xs(), Empty)))
            case (Empty, Cons(y, ys)) => Some((None, Some(y())), (Empty, ys()))
            case _ => None
        })
        
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match {
            case Some((a: A, s: S)) => cons(a, unfold(s)(f))
            case _ => Empty
        }
    
    //zipWith

    //zipAll
    
    
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

val x = Stream(1, 2, 3, 4, 5)
x.map("Its a %d!".format(_)).toList
x.take(3).toList
x.takeWhile(_ < 3).toList
x.zipAll(Stream("one", "two")).toList