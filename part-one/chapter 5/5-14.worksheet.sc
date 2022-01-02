sealed trait Stream[+A] {
    def toList: List[A] = 
        this match {
            case Empty => Nil
            case Cons(h, t) => h() :: t().toList
        }

    def zipWith[B](b: Stream[B]): Stream[(A, B)] =
        Stream.unfold((this, b))(_ match {
            case (Cons(x, xs), Cons(y, ys)) => Some((x(), y()), (xs(), ys()))
            case _ => None
        })

    def foldRight[B](z: => B)(f: (A, => B) => B): B = 
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z 
        }

    def startsWith[B](s: Stream[B]): Boolean =
        zipWith(s).foldRight(true)((a, b) => (a._1 == a._2) && b)
        
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

val x = Stream(1, 2, 3, 4, 5)
x.startsWith(Stream(1, 2))
x.startsWith(Stream(2, 3))
x.startsWith(Stream(2, 1))
x.startsWith(Empty)
