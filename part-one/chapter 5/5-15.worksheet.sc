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

    def zipAll[B](a2: Stream[B]): Stream[(Option[A], Option[B])] =
        Stream.unfold((this, a2))(_ match {
            case (Cons(x, xs), Cons(y, ys)) => Some(((Some(x()), Some(y())), (xs(), ys())))
            case (Cons(x, xs), Empty) => Some(((Some(x()), None), (xs(), Empty)))
            case (Empty, Cons(y, ys)) => Some((None, Some(y())), (Empty, ys()))
            case _ => None
        })

    def startsWith[A](s: Stream[A]): Boolean =
        zipAll(s).foldRight(true)((a, b) => a._2 == None || a._1 == a._2 && b)


    def exists(f: A => Boolean): Boolean =
        foldRight(false)((a, b) => f(a) || b)

    def hasSubsequence[A](s: Stream[A]): Boolean =
        tails exists (_ startsWith s)
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
x.tails.map(a => a.foldRight(0)(_ + _)).toList

x.hasSubsequence(Stream(2, 3))
x.hasSubsequence(Stream(5, 6))
x.hasSubsequence(Empty)

