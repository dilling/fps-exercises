trait RNG {
    def nextInt: (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {
    val int: Rand[Int] = _.nextInt

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
    
    def flatMap[A,B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            f(a)(rng2)
        }
    
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => rng => (f(a), rng))

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => 
            flatMap(rb)(b => rng => (f(a, b), rng))
        )
}

val rng = SimpleRNG(42)
