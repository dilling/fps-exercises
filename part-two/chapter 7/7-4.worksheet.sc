import java.util.concurrent.Executors
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import java.time.Duration
import java.time.Instant
import java.util.concurrent.Callable
import java.util.concurrent.TimeUnit
import java.util.concurrent.{ ExecutorService, Future }

type Par[A] = ExecutorService => Future[A]


object Par {
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
            def call = a(es).get
        })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def doAsync[A,B](f: A => B): A => Par[B] = 
        a => lazyUnit(f(a))
}

val double = (a: Int) => a * 2
double(4)

val asyncDouble = Par.doAsync(double)
val f = asyncDouble(5)

val es = Executors.newFixedThreadPool(10)
val run = Par.run(es)(f).get()
