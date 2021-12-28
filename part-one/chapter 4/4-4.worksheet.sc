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
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
            a.foldRight(Some(Nil): Option[List[A]])((a, b) =>
                a.flatMap(x => b.flatMap(y => Some(x :: y))))
}

val x = List(Some(1), Some(2), Some(3))
val y = List(Some(1), None, Some(3))
Option.sequence(x)
Option.sequence(y)