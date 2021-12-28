sealed trait Option[+A] {
    def flatMap[B](f: A => Option[B]): Option[B] =
        this match {
            case Some(x) => f(x)
            case None => None
        }
}
case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

object Option {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a.foldRight(Some(Nil): Option[List[B]])((a, b) => 
            b.flatMap(y => f(a).flatMap(x => Some(x :: y))))

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
        Option.traverse(a)(x => x)
}

val x = List(Some(1), Some(2), Some(3))
val y = List(Some(1), None, Some(3))
Option.sequence(x)
Option.sequence(y)