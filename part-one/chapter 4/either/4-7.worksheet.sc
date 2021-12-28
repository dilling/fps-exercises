sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
        this match {
            case Right(x) => Right(f(x))
            case Left(e) => Left(e)
        }
    
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
            case Right(x) => f(x)
            case Left(e) => Left(e)
        }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
    def traverse[E, B, C](a: List[B])(f: B => Either[E, C]): Either[E, List[C]] =
        a.foldRight(Right(Nil): Either[E, List[C]])((a, b) => b.flatMap(xs => f(a).map(x => x :: xs)))

    def sequence[E, B](a: List[Either[E, B]]): Either[E, List[B]] = 
        Either.traverse(a)(x => x)
}