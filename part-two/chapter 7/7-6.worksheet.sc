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

    private def convertTimeUnit(u: TimeUnit): TemporalUnit = 
        u match {
            case TimeUnit.DAYS => ChronoUnit.DAYS
            case TimeUnit.HOURS => ChronoUnit.HOURS
            case TimeUnit.MICROSECONDS => ChronoUnit.MICROS
            case TimeUnit.MILLISECONDS => ChronoUnit.MILLIS
            case TimeUnit.MINUTES => ChronoUnit.MINUTES
            case TimeUnit.NANOSECONDS => ChronoUnit.NANOS
            case TimeUnit.SECONDS => ChronoUnit.SECONDS
        }

    private case class MapFuture[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C) extends Future[C] {
        def isDone = af.isDone && bf.isDone
        def get() = f(af.get, bf.get)
        def get(timeout: Long, units: TimeUnit) = {
            val start = Instant.now
            val a = af.get(timeout, units)
            val duration = Duration.between(start, Instant.now)

            val timeRemaining = timeout - duration.get(convertTimeUnit(units))
            val b = bf.get(timeRemaining, units)
            f(a, b)
        }
        def isCancelled = af.isCancelled || bf.isCancelled
        def cancel(evenIfRunning: Boolean): Boolean = {
            if (isDone || isCancelled) false
            else af.cancel(evenIfRunning) && bf.cancel(evenIfRunning)
        }
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
        (es: ExecutorService) => {
            val af = a(es)
            val bf = b(es)
            MapFuture(af, bf, f)
        }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a,_) => f(a))

    def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
            def call = a(es).get
        })


    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def doAsync[A,B](f: A => B): A => Par[B] = 
        a => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(unit(Nil): Par[List[A]])((a, b) => map2(a, b)(_ :: _))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(doAsync(f))
        sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
        val asyncF = doAsync(f)
        as.map(asyncF).zip(as).foldRight(unit(Nil): Par[List[A]])(
            (a, b) => map2(a._1, b)((x, y) => if x then a._2 :: y else y)
        )
    }

    def divideAndConquer[A](z: A)(ints: IndexedSeq[A])(f: (A, A) => A): Par[A] =
        if(ints.length <= 1)
            unit(ints.headOption getOrElse z)
        else {
            val (l, r) = ints.splitAt(ints.length / 2)
            map2(Par.fork(divideAndConquer(z)(l)(f)), Par.fork(divideAndConquer(z)(r)(f)))(f)
        }

    def sum(ints: IndexedSeq[Int]): Par[Int] =
        divideAndConquer(0)(ints)(_ + _)

    def max(ints: IndexedSeq[Int]): Par[Int] =
        divideAndConquer(0)(ints)(_.max(_))
}

val ls = List(1, 2, 3, 4, 5)

val f = Par.parFilter(ls)(_ % 2 == 0)

val es = Executors.newFixedThreadPool(10)
val run = Par.run(es)(f).get()

val iSeq = IndexedSeq(1, 2, 3, 4, 5)
Par.run(es)(Par.sum(iSeq)).get()
Par.run(es)(Par.max(iSeq)).get()

