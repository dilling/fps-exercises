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
    
    def nonNegativeLessThan(n: Int): Rand[Int] = 
        flatMap(nonNegativeInt)(i => {
            val mod = i % n
            if (i + (n-1) - mod >= 0) rng => (mod, rng)
            else nonNegativeLessThan(n)
        })
}

val rng = SimpleRNG(42)
val (x1, rng1) = rng.nonNegativeLessThan(10000)(rng)
rng.nonNegativeLessThan(10000)(rng1)

