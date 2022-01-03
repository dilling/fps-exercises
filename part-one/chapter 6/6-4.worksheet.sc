trait RNG {
    def nextInt: (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {
    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(n: Int, acc: List[Int])(r: RNG): (List[Int], RNG) =
            if (n > 0) {
                val (i, r2) = r.nextInt
                go(n - 1, i :: acc)(r2)
            }
            else (acc, r)
        
        go(count, Nil)(rng)
    }
}

val rng = SimpleRNG(42)
val ints = rng.ints(3)(rng)