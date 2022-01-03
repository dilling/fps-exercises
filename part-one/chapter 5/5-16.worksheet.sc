sealed trait Stream[+A] {
    def toList: List[A] = 
        this match {
            case Empty => Nil
            case Cons(h, t) => h() :: t().toList
        }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = 
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z 
        }

    def map[B](f: A => B): Stream[B] = 
        Stream.unfold(this)(_ match {
            case Cons(h, t) => Some(f(h()), t())
            case _ => None
        })
        

    def tails: Stream[Stream[A]] = 
        this match {
            case Cons(h, t) => Stream.cons(this, t().tails)
            case _ => Empty
        }

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = 
        this.tails.map(_.foldRight(z)(f))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match {
            case Some((a: A, s: S)) => cons(a, unfold(s)(f))
            case _ => Empty
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

val x = Stream(1, 2, 3)

x.scanRight(0)(_ + _).toList


