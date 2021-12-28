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
    def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        a.foldRight(Right(Nil): Either[E, List[B]])((a, b) => b.flatMap(xs => f(a).map(x => x :: xs)))

    def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = 
        Either.traverse(a)(x => x)
}