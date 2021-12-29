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
        this.foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
    
    def filter(f: A => Boolean): Stream[A]=
        this.foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)

    
    def append[AA >: A](x: => Stream[AA]): Stream[AA] = 
        this.foldRight(x)((a, b) => Cons(() => a, () => b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = 
        foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
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
x.filter(_ % 2 == 0).toList
x.append(Stream(6, 7, 8)).toList
x.flatMap(x => Stream(x, x)).toList

