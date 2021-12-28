sealed trait Option[+A] {
    def flatMap[B](f: A => Option[B]): Option[B] =
        this match {
            case Some(x) => f(x)
            case None => None
        }
}
case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

object main {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        a.flatMap(x => b.flatMap(y => Some(f(x,y))))
    }
}

val x: Option[Int] = Some(1)
val y: Option[Int] = Some(2)
val z: Option[Int] = None

main.map2(x, y)(_ + _)
main.map2(x, z)(_ + _)
