trait RNG {
    def nextInt: (Int, RNG)
    def nonNegativeInt(rng: RNG): (Int, RNG)
    def double: Rand[Double]
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

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        rng => {
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng2)
            (f(a, b), rng3)
        }
    
    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
        map2(ra, rb)((_,_))

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
        map(nonNegativeInt)(_.toDouble / Int.MaxValue)

    def intDouble: Rand[(Double, Int)] = 
        both(double, int)

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (d, rng2) = rng.double(rng)
        val (i, rng3) = rng2.nextInt
        ((d, i), rng3)
    }
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng2) = rng.double(rng)
        val (d2, rng3) = rng2.double(rng2)
        val (d3, rng4) = rng3.double(rng3)
        ((d1, d2, d3), rng4)
    }
}

val rng = SimpleRNG(42)
rng.doubleInt(rng)
