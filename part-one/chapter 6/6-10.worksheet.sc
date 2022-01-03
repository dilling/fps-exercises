case class State[S,+A](run: S => (A,S)) {
    def unit[A](a: A): State[S,A] =
        State(s => (a, s))

    def flatMap[B](f: A => State[S,B]): State[S, B] =
        State(s => {
            val (a, s2) = run(s)
            f(a).run(s2)
        })

    def map[B](f: A => B): State[S, B] =
        flatMap(a => State(s => (f(a), s)))

    def map2[B,C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(a => 
            rb.flatMap(b => State(s => (f(a, b), s)))
        )
}

object State {
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
        State(s => 
            fs.foldLeft((Nil: List[A], s))((acc, curState) => {
                val (a, nextState) = curState.run(acc._2)
                (a :: acc._1, nextState)
            }))
}

trait RNG {
    def nextInt: (Int, RNG)
}

type Rand[A] = State[RNG, A]

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) =
        rng.nextInt match {
            case (Int.MinValue, next) => nonNegativeInt(next)
            case (x, next) if x < 0 => (-x, next)
            case (x, next) => (x, next)
        }

    def double: Rand[Double] =
        State(nonNegativeInt).map(_.toDouble / Int.MaxValue)

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        val fx = List.fill(count)(State((x: RNG) => x.nextInt))
        State.sequence(fx).run(rng)
}

val rng = SimpleRNG(42)

val (x1, rng2) = rng.double.run(rng)
rng.double.run(rng)
rng.double.run(rng2)
rng.ints(5)(rng2)