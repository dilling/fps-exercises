import java.util.concurrent.ExecutorService

sealed trait Par[A] {
    def unit[A](a: A): Par[A] 
    def run[A](a: Par[A]): A
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
    def fork[A](a: => Par[A]): Par[A]
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
}

// case class FuturePar[A](a: A) extends Par[Future[A]] {
//     def unit[A](a: A)(implicit ec: ExecutionContext): Par[Future[A]] = FuturePar(Future { a })
//     def fork[A](a: => Par[A])(implicit ec: ExecutionContext): Par[A] = FuturePar(Future { a })

//     def run[A](a: Par[A]): Fu
// }
