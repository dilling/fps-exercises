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

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
        this match {
            case Right(x) => Right(x)
            case _ => b
        }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
        (this, b) match {
            case (Right(x), Right(y)) => Right(f(x,y))
            case (Left(e), _) => Left(e)
            case (_, Left(e)) => Left(e)
        }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]